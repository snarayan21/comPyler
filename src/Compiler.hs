{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler where 

import Language.Python.Common.AST as AST (
    StatementSpan, 
    Statement (..),
    Expr (..),
    ExprSpan,
    Ident (..),
    OpSpan,
    Op (..),
    Argument (..),
    ArgumentSpan,
    DictKeyDatumList (..))
import Types (getSortedIdents, CodeState (..), Instruction (..), Identifier, Arg(..), VarContext(..), OpcodeType (..), Opcode(..), opcodeList, CallArgs(..), initCallArgs)
import Data.Word
import Scope
import Control.Monad
import Control.Monad.State as S
import qualified Data.Map as Map 
import Data.Set as Set (toList) 
import Writer (PyCodeObj(..), PycFile(..))
import qualified Data.ByteString.Lazy as B (pack, ByteString)
import Data.Bits (shiftL, (.|.)) 


class Executable a where 
    exec :: a -> StateT CodeState IO ()

instance Executable [StatementSpan] where 
    exec stmts = do mapM_ exec stmts

instance Executable StatementSpan where 
    exec = compileStatement 

instance Executable ExprSpan where
    exec = compileExpression

instance Executable ArgumentSpan where
    exec (ArgExpr {..}) = exec arg_expr
    exec _ = error "argument span"

io :: IO a -> StateT CodeState IO a
io = liftIO

incrementState :: StateT CodeState IO Int
incrementState = do 
    cstate <- S.get
    let newState = cstate { stateLabel = stateLabel cstate + 1 }
    S.put newState
    return $ stateLabel newState

compileAssignment :: ExprSpan -> StateT CodeState IO ()
compileAssignment (Var {..}) = do 
    instruction <- generateStoreInstruction (ident_string var_ident)
    writeInstruction instruction
compileAssignment (AST.List {..}) = do
    mapM_ compileAssignment list_exprs
compileAssignment (Subscript { .. }) = do 
    exec subscriptee
    exec subscript_expr
    instruction <- generateNoArgInstruction STORE_SUBSCR
    writeInstruction instruction
compileAssignment _ = error "compile assignment"

compileStatement :: StatementSpan -> StateT CodeState IO ()
compileStatement (Assign { .. }) = do 
    exec assign_expr -- compiles the RHS of the expression
    compileAssignment $ head assign_to -- we only support single assignments for now
compileStatement (StmtExpr { .. }) = do 
    -- TODO: ignore pure expressions
    exec stmt_expr
    instruction <- generateNoArgInstruction POP_TOP
    writeInstruction instruction
compileStatement (Return { return_expr = Nothing }) = do
    instruction <- generateConstantInstruction Writer.None
    writeInstruction instruction
    writeInstruction $ Instruction {opcode = RETURN_VALUE, arg = Nothing} 
compileStatement (Return { return_expr = Just x }) = do 
    exec x 
    writeInstruction $ Instruction {opcode = RETURN_VALUE, arg = Nothing} 
compileStatement (For { .. }) = do
    start <- incrementState
    -- execuate generator
    exec for_generator 
    -- further loop setup
    getIterInstruction <- generateGenericInstruction GET_ITER (Arg $ (fromIntegral :: Int -> Word8) start)
    writeInstruction getIterInstruction
    forIterInstruction <- generateGenericInstruction FOR_ITER (Arg $ (fromIntegral :: Int -> Word8) (length for_body))
    writeInstruction forIterInstruction
    cstate <- S.get 
    let forIterLoc = length (instructions cstate) - 1
    -- execute body
    compileAssignment $ head for_targets -- for now, we only support single assignments
    mapM_ exec for_body
    -- jump back to start
    jumpInstruction <- generateGenericInstruction JUMP_ABSOLUTE (Arg $ (fromIntegral :: Int -> Word8) forIterLoc)
    writeInstruction jumpInstruction
    custate <- S.get 
    let jumpLoc = length (instructions custate)
        actualForIterLoc = jumpLoc - forIterLoc - 1
        (x,_:ys) = splitAt forIterLoc (instructions custate)   
        arg = (fromIntegral :: Int -> Word8) actualForIterLoc     
        newState = x ++ Instruction FOR_ITER (Just (Arg arg)) : ys
        newState' = custate { instructions = newState }
    S.put newState' 
compileStatement (While { .. }) = do 
    cstate <- S.get 
    let jumpIfFalseLoc = length (instructions cstate)
    -- execute condition
    exec while_cond
    -- further loop setup
    jumpIfFalseInstruction <- generateGenericInstruction POP_JUMP_IF_FALSE (Arg $ (fromIntegral :: Int -> Word8) (length while_body))
    writeInstruction jumpIfFalseInstruction
    -- execute body
    mapM_ exec while_body
    -- jump back to start
    exec while_cond 
    jumpInstruction <- generateGenericInstruction POP_JUMP_IF_TRUE (Arg $ (fromIntegral :: Int -> Word8) jumpIfFalseLoc)
    writeInstruction jumpInstruction
compileStatement (Conditional {..}) = do
    currentstate <- S.get
    let nestlv = nestlevel currentstate
    S.put currentstate {nestlevel = nestlv + 1}
    mapM_ (compileGuard (nestlv + 1)) cond_guards
    compileElse (nestlv + 1) cond_else
    where 
        -- Compile a conditional guard
        compileGuard :: Int -> (ExprSpan, [StatementSpan]) -> StateT CodeState IO ()
        compileGuard nest (expr, stmts) = do
            exec expr
            cstate <- S.get
            let originalLoc = length (instructions cstate) - 1      
            jumpInstr <- generateGenericInstruction POP_JUMP_IF_FALSE (Arg $ (fromIntegral :: Int -> Word8) 0)
            writeInstruction jumpInstr
            mapM_ exec stmts
            writeInstruction $ Instruction {opcode = JUMP_FORWARD, arg = Just $ Arg $ (fromIntegral :: Int -> Word8) 0}
            custate <- S.get 
            let newLoc = length (instructions custate)
                condLocList = Map.lookup nest (conditional_locs custate)
                (x,_:ys) = splitAt (originalLoc + 1) (instructions custate)   
                arg = (fromIntegral :: Int -> Word8) newLoc
                newState = x ++ Instruction POP_JUMP_IF_FALSE (Just (Arg arg)) : ys
            case condLocList of
                Just cond_list -> do 
                    let new_cond_locs = newLoc : cond_list
                        newState' = custate { instructions = newState, conditional_locs = Map.insert nest new_cond_locs (conditional_locs custate)}
                    S.put newState'
                Nothing -> do
                    let new_cond_locs = [newLoc]
                        newState' = custate {instructions = newState, conditional_locs = Map.insert nest new_cond_locs (conditional_locs custate)}
                    S.put newState'            
        
        compileElse :: Int -> [StatementSpan] -> StateT CodeState IO ()
        compileElse nest stmts = do
            mapM_ exec stmts
            custate <- S.get
            let jumpLoc = length (instructions custate)
                condLocList = Map.lookup nest (conditional_locs custate)
            updateJumpForwards jumpLoc condLocList
            curstate <- S.get
            let finalState = curstate {nestlevel = nest - 1, conditional_locs = Map.insert nest [] (conditional_locs curstate)}
            S.put finalState
            where
                updateJumpForwards :: Int -> Maybe [Int] -> StateT CodeState IO ()
                updateJumpForwards _ Nothing = return ()
                updateJumpForwards _ (Just []) = return ()
                updateJumpForwards jumpl (Just (x:xs)) = do
                    cstate <- S.get
                    let (bef, _:aft) = splitAt (x-1) (instructions cstate)
                        offset = jumpl - x
                        newInsts = bef ++ Instruction {opcode = JUMP_FORWARD, arg = Just $ Arg $ (fromIntegral :: Int -> Word8) offset} : aft
                        newState = cstate {instructions = newInsts}
                    S.put newState
                    updateJumpForwards jumpl (Just xs)            
compileStatement (AST.Fun { .. }) = do
    custate <- S.get 
    -- update context to Function
    let newState = custate { varContext = Function }
    S.put newState
    -- compile function scope
    scope <- compileScope fun_body
    argScope <- compileScope fun_args
    mapM_ lookupAndMaybeWriteFastVar (assigned argScope <> Set.toList (referenced scope))
    curstate <- S.get 
    let preFunctionBodyLoc = length (instructions curstate)
    -- compile function body
    exec fun_body
    currstate <- S.get
    let (x,ys) = splitAt preFunctionBodyLoc (instructions currstate)
        newState' = currstate { instructions = x}
    S.put newState'
    -- append return none to ys if no return 
    -- make pycodeobj
    let numParams = (fromIntegral :: Int -> Word32) (length fun_args)
        returnInstr = Instruction {opcode = RETURN_VALUE, arg = Nothing}
    -- preprocess ys: subtract from jump absolute offsets to correct for iter
    noneInstr <- generateConstantInstruction Writer.None
    let processedYs = processJumpAbs preFunctionBodyLoc ys
    obj <- do if findReturn processedYs then makePyCodeObj (ident_string fun_name) processedYs numParams else makePyCodeObj (ident_string fun_name) (processedYs ++ [noneInstr, returnInstr]) numParams
    pyCodeObjInstruction <- generateConstantInstruction obj
    writeInstruction pyCodeObjInstruction
    cstate <- S.get 
    S.put cstate { fastVars = Map.empty }
    -- count number of default params 
    -- compile closure 
    closureInstructions <- generateClosureInstructions (ident_string fun_name) ((fromIntegral :: Int -> Word8) 0)
    mapM_ writeInstruction closureInstructions
    -- write function name
    instruction <- generateStoreInstruction (ident_string fun_name)
    writeInstruction instruction
    where 
        findReturn :: [Instruction] -> Bool 
        findReturn [] = False
        findReturn (x:xs) = case x of 
            Instruction {opcode = RETURN_VALUE} -> True
            _ -> findReturn xs

        processJumpAbs :: Int -> [Instruction] -> [Instruction]
        processJumpAbs _ [] = []
        processJumpAbs sub (x:xs) = case x of 
            Instruction {opcode = JUMP_ABSOLUTE, arg = Just (Arg arg)} -> Instruction {opcode = JUMP_ABSOLUTE, arg = Just (Arg (arg - (fromIntegral :: Int -> Word8) sub))} : processJumpAbs sub xs
            _ -> x : processJumpAbs sub xs
compileStatement _ = error "compileStatement"

-- note that this is called in context of exec, so the opcode variant is passed
compileExpression :: ExprSpan -> StateT CodeState IO ()
compileExpression (Var { var_ident = ident } ) = do
    instruction <- generateLoadInstruction (ident_string ident)
    writeInstruction instruction
compileExpression (Call { .. }) = do
    exec call_fun
    instruction <- generateCallInstruction call_args
    writeInstruction instruction
compileExpression (AST.CondExpr {..}) = do
   exec ce_condition
   falseLabel <- incrementState
   popInstr <- generateGenericInstruction POP_JUMP_IF_FALSE (Arg $ (fromIntegral :: Int -> Word8) falseLabel)
   writeInstruction popInstr
   exec ce_true_branch
   restLabel <- incrementState 
   fwrdInstr <- generateGenericInstruction JUMP_FORWARD (Arg $ (fromIntegral :: Int -> Word8) restLabel)
   writeInstruction fwrdInstr
   exec ce_false_branch
compileExpression (AST.Int { .. }) = do
    instruction <- generateConstantInstruction $ Writer.Int { int_val = (fromIntegral :: Integer -> Word32) int_value }
    writeInstruction instruction
compileExpression (AST.Float { .. }) = do
    instruction <- generateConstantInstruction $ Writer.Float { float_val = float_value }
    writeInstruction instruction
compileExpression (AST.Strings { .. }) = do
    instruction <- generateConstantInstruction $ Writer.Unicode { unicode = concatAndNormalizeString }
    writeInstruction instruction
    where 
        -- remove original quotes and normalize slashes
        concatAndNormalizeString :: String
        concatAndNormalizeString = concatMap (normalizeString . init . tail) strings_strings
        normalizeString :: String -> String
        normalizeString = map (\c -> if c == '\\' then '/' else c)
compileExpression (AST.Bool { .. }) = do
    if bool_value 
        then do 
            instruction <- generateConstantInstruction Writer.TrueVal
            writeInstruction instruction
        else do 
            instruction <- generateConstantInstruction Writer.FalseVal
            writeInstruction instruction
compileExpression (Dictionary { .. }) = do 
    instruction <- generateGenericInstruction BUILD_MAP (Arg $ (fromIntegral :: Int -> Word8) (length dict_mappings))
    writeInstruction instruction
    forM_ dict_mappings $ \(DictMappingPair key value) -> do
        exec key
        exec value
        inst <- generateNoArgInstruction STORE_MAP
        writeInstruction inst
compileExpression (AST.Set { .. }) = do 
    mapM_ exec set_exprs    
    instruction <- generateGenericInstruction BUILD_SET (Arg $ (fromIntegral :: Int -> Word8) (length set_exprs))
    writeInstruction instruction
compileExpression (AST.Dot { .. }) = do
    exec dot_expr 
    arg <- lookupAndMaybeWriteName (ident_string dot_attribute)
    instruction <- generateGenericInstruction LOAD_METHOD arg
    writeInstruction instruction
    cstate <- S.get 
    S.put cstate { isInDot = True }
compileExpression (BinaryOp {..}) = do 
    exec left_op_arg
    let isBool = isBoolean operator
    currstate <- S.get 
    let preBooleanLoc = length (instructions currstate)
    when isBool $ do
        case operator of
            And _ -> do
                instruction <- generateGenericInstruction JUMP_IF_FALSE_OR_POP (Arg $ (fromIntegral :: Int -> Word8) 0)
                writeInstruction instruction
            Or _ -> do
                instruction <- generateGenericInstruction JUMP_IF_TRUE_OR_POP (Arg $ (fromIntegral :: Int -> Word8) 0)
                writeInstruction instruction
            _ -> error "Unexpected operator!" 
    exec right_op_arg 
    when isBool $ do 
        cstate <- S.get
        let rightOpInstructions = length (instructions cstate)
            (x,_:ys) = splitAt preBooleanLoc (instructions cstate)  
            arg = (fromIntegral :: Int -> Word8) rightOpInstructions    
        case operator of 
            And _ -> do 
                let newState = x ++ Instruction JUMP_IF_FALSE_OR_POP (Just (Arg arg)) : ys
                    newState' = cstate { instructions = newState }
                S.put newState' 
            Or _ -> do 
                let newState = x ++ Instruction JUMP_IF_TRUE_OR_POP (Just (Arg arg)) : ys
                    newState' = cstate { instructions = newState }
                S.put newState' 
            _ -> error "Unexpected operator!"
    unless isBool $ do
        instruction <- generateBinaryOpInstruction operator
        writeInstruction instruction
    where 
        isBoolean :: OpSpan -> Bool
        isBoolean (And {}) = True
        isBoolean (Or {}) = True
        isBoolean _other = False
compileExpression (UnaryOp {..}) = do 
    exec op_arg
    instruction <- generateUnaryOpInstruction operator
    writeInstruction instruction
compileExpression (Paren {..}) = compileExpression paren_expr
compileExpression (Subscript { .. }) = do 
    exec subscriptee
    exec subscript_expr 
    instruction <- generateNoArgInstruction BINARY_SUBSCR
    writeInstruction instruction
compileExpression (List { .. }) = do 
    mapM_ exec list_exprs
    instruction <- generateGenericInstruction BUILD_LIST (Arg $ (fromIntegral :: Int -> Word8) (length list_exprs))
    writeInstruction instruction        
compileExpression _ = do 
    io $ print "Unmatched"

generateNoArgInstruction :: Opcode -> StateT CodeState IO Instruction
generateNoArgInstruction op = return $ Instruction op Nothing    

generateGenericInstruction :: Opcode -> Arg -> StateT CodeState IO Instruction
generateGenericInstruction op arg = return $ Instruction op (Just arg)

generateBinaryOpInstruction :: OpSpan -> StateT CodeState IO Instruction
generateBinaryOpInstruction op = 
    case op of
        Plus _ -> return $ Instruction BINARY_ADD Nothing
        Minus _ -> return $ Instruction BINARY_SUBTRACT Nothing
        Multiply _ -> return $ Instruction BINARY_MULTIPLY Nothing
        Divide _ -> return $ Instruction BINARY_TRUE_DIVIDE Nothing
        FloorDivide _ -> return $ Instruction BINARY_FLOOR_DIVIDE Nothing
        LessThan _ -> return $ Instruction COMPARE_OP (Just $ Arg 0)
        LessThanEquals _ -> return $ Instruction COMPARE_OP (Just $ Arg 1)
        Equality _ -> return $ Instruction COMPARE_OP (Just $ Arg 2)
        NotEquals _ -> return $ Instruction COMPARE_OP (Just $ Arg 3)
        GreaterThan _ -> return $ Instruction COMPARE_OP (Just $ Arg 4)
        GreaterThanEquals _ -> return $ Instruction COMPARE_OP (Just $ Arg 5)
        In _ -> return $ Instruction COMPARE_OP (Just $ Arg 6)
        NotIn _ -> return $ Instruction COMPARE_OP (Just $ Arg 7)
        Is _ -> return $ Instruction COMPARE_OP (Just $ Arg 8)
        IsNot _ -> return $ Instruction COMPARE_OP (Just $ Arg 9)
        _ -> error "generateBinaryOpInstruction"

generateUnaryOpInstruction :: OpSpan -> StateT CodeState IO Instruction
generateUnaryOpInstruction op =
    case op of
        Plus _ -> return $ Instruction UNARY_POSITIVE Nothing
        Minus _ -> return $ Instruction UNARY_NEGATIVE Nothing
        Not _ -> return $ Instruction UNARY_NOT Nothing
        _ -> error "generateUnaryOpInstruction"

generateConstantInstruction :: PyCodeObj -> StateT CodeState IO Instruction
generateConstantInstruction obj = do 
    constIndex <- lookupAndMaybeWriteConstant obj
    return $ Instruction LOAD_CONST (Just constIndex)

generateClosureInstructions :: String -> Word8 -> StateT CodeState IO [Instruction]
generateClosureInstructions name numParams = do 
    nameConstInstruction <- generateConstantInstruction $ Writer.Unicode { unicode = name }
    makeFunctionInstruction <- generateGenericInstruction MAKE_FUNCTION (Arg numParams)
    return [nameConstInstruction, makeFunctionInstruction]

generateLoadInstruction :: Identifier -> StateT CodeState IO Instruction
generateLoadInstruction ident = do 
    cstate <- S.get 
    (opcodeType, arg) <- getOpcodeType (varContext cstate) ident
    case opcodeType of 
        Deref -> return $ Instruction LOAD_DEREF arg
        Fast -> return $ Instruction LOAD_FAST arg
        Name -> return $ Instruction LOAD_NAME arg
        Types.Global -> return $ Instruction LOAD_GLOBAL arg
        _ -> error "generateLoadInstruction"

generateStoreInstruction :: Identifier -> StateT CodeState IO Instruction
generateStoreInstruction ident = do 
    cstate <- S.get 
    (opcodeType, arg) <- getOpcodeType (varContext cstate) ident
    case opcodeType of 
        Deref -> return $ Instruction STORE_DEREF arg
        Fast -> return $ Instruction STORE_FAST arg
        Name -> return $ Instruction STORE_NAME arg
        Types.Global -> return $ Instruction STORE_GLOBAL arg
        _ -> error "generateStoreInstruction"

generateDeleteInstruction :: Identifier -> StateT CodeState IO Instruction
generateDeleteInstruction ident = do 
    cstate <- S.get 
    (opcodeType, arg) <- getOpcodeType (varContext cstate) ident
    case opcodeType of 
        Deref -> return $ Instruction DELETE_DEREF arg
        Fast -> return $ Instruction DELETE_FAST arg
        Name -> return $ Instruction DELETE_NAME arg
        Types.Global -> return $ Instruction DELETE_GLOBAL arg 
        _ -> error "generateDeleteInstruction"    

generateCallInstruction :: [ArgumentSpan] -> StateT CodeState IO Instruction
generateCallInstruction args = do
    callArgs <- aux args 
    currstate <- S.get
    let arg = callArgs_pos callArgs .|. (callArgs_keyword callArgs `shiftL` 8)
        isDot = isInDot currstate
    cstate <- S.get 
    if isDot then do
        S.put cstate { isInDot = False }
        return (Instruction CALL_METHOD $ Just (Arg arg)) 
    else return (Instruction CALL_FUNCTION $ Just (Arg arg))
    where
        aux :: [ArgumentSpan] -> StateT CodeState IO CallArgs
        aux = foldM getArgs initCallArgs
            where
            getArgs :: CallArgs -> ArgumentSpan -> StateT CodeState IO CallArgs 
            getArgs callArgs@(CallArgs { .. }) (ArgExpr { .. }) = do
                exec arg_expr
                return $ callArgs { callArgs_pos = callArgs_pos + 1 }
            getArgs callArgs@(CallArgs { .. }) (ArgKeyword { .. }) = do
                instruction <- generateConstantInstruction $ Writer.Unicode $ ident_string arg_keyword
                writeInstruction instruction
                exec arg_expr
                return $ callArgs { callArgs_keyword = callArgs_keyword + 1 }  
            getArgs _ _ = error "getArgs generateCallInstruction" 

writeInstruction :: Instruction -> StateT CodeState IO ()
writeInstruction instruction = do 
    cstate <- S.get 
    let newCode = instructions cstate ++ [instruction]
    S.put $ cstate { instructions = newCode }

-- this performs a lookup in the currently existing names;
-- if a variable does not exist, we add it both to the map and the list of names 
-- (which is eventually used in the writer)
lookupAndMaybeWriteName :: Identifier -> StateT CodeState IO Arg
lookupAndMaybeWriteName ident = do 
    cstate <- S.get
    let m = namesMap cstate
    case Map.lookup ident m of 
        Just index -> return index
        Nothing -> do 
            let index = Arg $ (fromIntegral :: Int -> Word8) (Map.size m)
                m' = Map.insert ident index m
                newNames = ident : namesArr cstate
            S.put $ cstate { namesMap = m', namesArr = newNames }
            return index

lookupAndMaybeWriteFastVar :: Identifier -> StateT CodeState IO Arg
lookupAndMaybeWriteFastVar ident = do 
    cstate <- S.get
    let m = fastVars cstate
    case Map.lookup ident m of 
        Just index -> return index
        Nothing -> do 
            let index = Arg $ (fromIntegral :: Int -> Word8) (Map.size m)
                m' = Map.insert ident index m
            S.put $ cstate { fastVars = m' }
            return index

lookupAndMaybeWriteConstant :: PyCodeObj -> StateT CodeState IO Arg
lookupAndMaybeWriteConstant obj = do 
    cstate <- S.get 
    let m = constantsMap cstate
    case Map.lookup obj m of 
        Just index -> return index
        Nothing -> do 
            let index = Arg $ (fromIntegral :: Int -> Word8) (Map.size m)
                m' = Map.insert obj index m
                newConstants = obj : constantsArr cstate
            S.put $ cstate { constantsMap = m', constantsArr = newConstants }
            return index

-- https://tenthousandmeters.com/blog/python-behind-the-scenes-5-how-variables-are-implemented-in-cpython/
-- given a current context, determine the type of opcode instruction to output,
-- as well as an optional argument index (for cell vars)
getOpcodeType :: VarContext -> Identifier -> StateT CodeState IO (OpcodeType, Maybe Arg)
getOpcodeType Types.Module ident = do 
    -- in a module context, we have access to the variables in co_names
    idx <- lookupAndMaybeWriteName ident 
    return (Name, Just idx)
getOpcodeType Types.Function ident = do
    -- in a function context, we need to check whether the variable is local, free, or global
    cstate <- S.get 
    -- https://github.com/universe-proton/universe-topology/issues/15#:~:text=The%20free%20variable%20is%20a,variable%20in%20the%20outside%20function.
    -- the free variable is a variable which refers to the value in cell variables of the outside function
    -- the cell variable is referred in the inside function as a part of the closure
    let cells = cellVars cstate
    let fast = fastVars cstate
    let free = freeVars cstate 
    let global = globalVars cstate
    case (Map.lookup ident cells, Map.lookup ident fast, Map.lookup ident free, Map.lookup ident global) of 
        (Just idx, _, _, _) -> return (Deref, Just idx)
        (_, Just idx, _, _) -> return (Fast, Just idx)
        (_, _, Just idx, _) -> return (Deref, Just idx)
        (_, _, _, Just idx) -> return (Types.Global, Just idx)
        -- unknown variable
        _ -> getOpcodeType Types.Module ident

makePycFile :: Integer -> Integer -> StateT CodeState IO PycFile
makePycFile fsize magicnum = do
    cstate <- S.get
    let code_instructions = instructions cstate
    let fixedStackSize = 10 --allocating a fixed stack size. is inefficient...tuff
    let state_names = namesArr cstate
    let state_constants = constantsArr cstate
    let state_frees = freeVars cstate
    let state_cells = cellVars cstate
    let state_argcount = 0 --not supporting input args
    let state_kwargcount = 0 --not supporting input kwargs
    let state_flags = (64 :: Word32) --TODO: DO FLAGS FOR REAL
    let state_fasts = Map.empty --fastVars state 
    let firstl = (1 :: Word32) --assuming the first line is always line 1
    let localvaridents = map Writer.Unicode (getSortedIdents state_fasts)
    let fname = "simple.py" --TODO: update filename in state fr!
    let oname = "<module>" --TODO: update object name in state fr!
    lnotablestring <- getLineNoTable
    let codeobj = Writer.Code {
        argcount = state_argcount,
        kwargcount = state_kwargcount,
        numlocals = fromIntegral (length localvaridents),
        stacksize = fixedStackSize,
        codeflags = state_flags,
        code = Writer.String (encodeInstructions code_instructions),
        consts = Writer.Tuple (reverse state_constants),
        names = Writer.Tuple (map Writer.Unicode (reverse state_names)),
        varnames = Writer.Tuple localvaridents,
        freevars = Writer.Tuple (map Writer.Unicode (getSortedIdents state_frees)),
        cellvars = Writer.Tuple (map Writer.Unicode (getSortedIdents state_cells)),
        codefilename = Writer.Unicode fname,
        name = Writer.Unicode oname,
        firstline = firstl,
        lnotable = lnotablestring
    }
    let mod_t = (0 :: Word32) --assuming mod time of 0
    let pycfile = PycFile {
        magic = fromIntegral magicnum,
        mod_time = mod_t,
        size = fromIntegral fsize,
        object = codeobj
    }
    return pycfile

makePyCodeObj :: String -> [Instruction] -> Word32 -> StateT CodeState IO PyCodeObj
makePyCodeObj funName instructions argCount = do
    cstate <- S.get
    let fixedStackSize = 10 --allocating a fixed stack size. is inefficient...tuff
    let cstate_names = namesArr cstate
    let cstate_constants = constantsArr cstate
    let cstate_frees = freeVars cstate
    let cstate_cells = cellVars cstate
    let cstate_kwargcount = 0 --not supporting input kwargs
    let cstate_flags = (64 :: Word32) --TODO: DO FLAGS FOR REAL
    let cstate_fasts = fastVars cstate
    let firstl = (1 :: Word32) --assuming the first line is always line 1
    let localvaridents = map Writer.Unicode (getSortedIdents cstate_fasts)
    let fname = "simple.py" --TODO: update filename in state fr!
    let oname = funName --TODO: update object name in state fr!
    lnotablestring <- getLineNoTable
    let codeobj = Writer.Code {
        argcount = argCount,
        kwargcount = cstate_kwargcount,
        numlocals = fromIntegral (length localvaridents),
        stacksize = fixedStackSize,
        codeflags = cstate_flags,
        code = Writer.String (encodeInstructions instructions),
        consts = Writer.Tuple (reverse cstate_constants),
        names = Writer.Tuple (map Writer.Unicode (reverse cstate_names)),
        varnames = Writer.Tuple localvaridents,
        freevars = Writer.Tuple (map Writer.Unicode (getSortedIdents cstate_frees)),
        cellvars = Writer.Tuple (map Writer.Unicode (getSortedIdents cstate_cells)),
        codefilename = Writer.Unicode fname,
        name = Writer.Unicode oname,
        firstline = firstl,
        lnotable = lnotablestring
    }
    return codeobj

encodeInstructions :: [Instruction] -> B.ByteString
encodeInstructions insts = B.pack (concat (map instructionToBytes insts))

opcodeValueMap :: Map.Map Opcode Word8
opcodeValueMap = Map.fromList opcodeList

valueOpcodeMap :: Map.Map Word8 Opcode
valueOpcodeMap = Map.fromList [(b,a) | (a,b) <- opcodeList]

instructionToBytes :: Instruction -> [Word8]
instructionToBytes (Instruction op arg) =
    case Map.lookup op opcodeValueMap of
        Nothing -> error "opcode wack."
        Just val -> case arg of
            Nothing -> [val, 0 :: Word8] --opcode with no arg has default arg 0.
            Just (Arg ar) -> [val, ar] --opcode with arg.

--last entry in pyc file is compressed line number table from bytecode to original source.
--this function computes it using first line number of 1 (assumption)
--and the lineNumber 
getLineNoTable :: StateT CodeState IO PyCodeObj
getLineNoTable = do
    cstate <- S.get
    let linestable = reverse (linenumtable cstate)
    let compressedlnotable = compressTable (0, 1) linestable
    let lnotablestring = B.pack (concat [[fromIntegral byteoffset, fromIntegral srcline] | (byteoffset, srcline) <- compressedlnotable])
    return Writer.String {string = lnotablestring}

--assumption here is that offsets and line numbers increase monotonically
--also, if one column jumps by more than 255 from one row to the next,
--we write more than one pair to the table. If byte offset increments by more than 256,
--line number increment should be 0 until the remaining byte offset increment is less than 256.
--see: https://svn.python.org/projects/python/branches/pep-0384/Objects/lnotab_notes.txt
compressTable :: (Word16, Word32) -> [(Word16, Word32)] -> [(Word16, Word32)]
compressTable _ [] = []
compressTable (curroff, currline) (new@(newoff, newline) : offlines) = 
    getdifferences (newoff-curroff, newline-currline) ++ compressTable new offlines where
        getdifferences :: (Word16, Word32) -> [(Word16, Word32)]
        getdifferences (offsetdiff, linediff) = 
            if offsetdiff < 256 then
                if linediff < 256 
                    then [(offsetdiff, linediff)] --chilling, both less than 256.
                else (offsetdiff, 255) : getdifferences (0, linediff-255) -- case when linediff >= but offset is less than 256
            else (255,0) : getdifferences (offsetdiff-255, linediff) --case when offsetdiff >= 256. we know linediff has to stay at 0 when this is the case.