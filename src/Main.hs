module Main (main, compileToPyc, compileToPycDCE, compileToInstructions, sourceToPycFileDCE, sourceToPycFile, sourceToPyCodeObj, mainAST) where

import Compiler
import Parser
import Writer
import Types
import Language.Python.Common.AST as AST 
import Control.Monad
import Control.Monad.State as S
import GenerateCFG (addIndexToInstructions, removeIndexFromInstructions, createBlockMap)
import DeadcodeElim (removeDeadCodeLines, getDeadCodeLines)
import LiveVariableAnalysis (calculateLivenessFinal)

main :: IO ()
main = undefined

--magicnum python 3.10.6: 168627567
compileToPyc :: String -> IO ()
compileToPyc pyfname = Control.Monad.void (runStateT (sourceToPycFile pyfname) (initCodeState pyfname))

compileToPycDCE :: String -> IO ()
compileToPycDCE pyfname = Control.Monad.void (runStateT (sourceToPycFileDCE pyfname) (initCodeState pyfname))

mainAST :: IO()
mainAST = do 
    ast <- Parser.parseFile "./python_utils/simple.py"
    print ast
    return ()

compileToInstructions :: FilePath -> StateT CodeState IO ()
compileToInstructions file = do
    ast <- lift $ Parser.parseFile file
    compileModule ast
    where 
        compileModule (AST.Module stmts) = do 
            mapM_ exec stmts
            -- at the end of a module, should return None
            instruction <- generateConstantInstruction Writer.None
            writeInstruction instruction
            writeInstruction $ Instruction {opcode = RETURN_VALUE, arg = Nothing}
            cstate <- S.get
            io $ print (instructions cstate)
            return ()

sourceToPycFileDCE :: FilePath -> StateT CodeState IO ()
sourceToPycFileDCE file = do
    ast <- lift $ Parser.parseFile file
    fsize <- lift $ Parser.getFileSize file
    compileModule ast fsize
    where 
        compileModule (AST.Module stmts) fsize  = do
            mapM_ exec stmts
            instruction <- generateConstantInstruction Writer.None
            writeInstruction instruction
            writeInstruction $ Instruction {opcode = RETURN_VALUE, arg = Nothing}            
            cstate <- S.get
            let instsInd = addIndexToInstructions (instructions cstate)
                liveness = calculateLivenessFinal instsInd
                dceInsts = removeIndexFromInstructions $ removeDeadCodeLines instsInd (getDeadCodeLines (createBlockMap instsInd) instsInd liveness)
                newstate = cstate {instructions =  dceInsts}
            S.put newstate
            obj <- makePycFile fsize 168627567
            io (writeToPycFile obj file)

sourceToPycFile :: FilePath -> StateT CodeState IO ()
sourceToPycFile file = do
    ast <- lift $ Parser.parseFile file
    fsize <- lift $ Parser.getFileSize file
    compileModule ast fsize
    where 
        compileModule (AST.Module stmts) fsize  = do
            mapM_ exec stmts
            instruction <- generateConstantInstruction Writer.None
            writeInstruction instruction
            writeInstruction $ Instruction {opcode = RETURN_VALUE, arg = Nothing}
            obj <- makePycFile fsize 168627567
            io (writeToPycFile obj file)

sourceToPyCodeObj :: Integer -> FilePath -> StateT CodeState IO ()
sourceToPyCodeObj magicnum file = do
    ast <- lift $ Parser.parseFile file
    fsize <- lift $ Parser.getFileSize file
    compileModule ast fsize
    where 
        compileModule (AST.Module stmts) fsize  = do
            mapM_ exec stmts
            instruction <- generateConstantInstruction Writer.None
            writeInstruction instruction
            writeInstruction $ Instruction {opcode = RETURN_VALUE, arg = Nothing}            
            obj <- makePycFile fsize magicnum
            io $ print obj