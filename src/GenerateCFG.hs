module GenerateCFG where 
import Data.Map (Map)
import qualified Data.Map as Map

import Types  
import Data.Word

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')
import qualified Data.List as List

import Test.HUnit hiding (State)
import Test.QuickCheck

data BytecodeSlice = 
    BytecodeSlice {
        start :: Int, 
        end :: Int
    } deriving (Show)

data Branch = Absolute | Relative deriving (Show)

data Block = Block {blockId :: Int, blockSlice :: BytecodeSlice} deriving (Show)
newtype BlockMap = BlockMap (Map Int Block) deriving (Show) -- mapping actual start index to block
newtype CFG = CFG (Map Int [Int]) deriving (Show) --mapping block start to list of block starts that are predecessors

getBranchType :: InstructionWInd -> Maybe Branch 
getBranchType ins = 
    case opc ins of 
        FOR_ITER -> Just Relative
        JUMP_FORWARD -> Just Relative
        JUMP_ABSOLUTE -> Just Absolute
        JUMP_IF_FALSE_OR_POP -> Just Absolute
        JUMP_IF_TRUE_OR_POP -> Just Absolute
        POP_JUMP_IF_FALSE -> Just Absolute
        POP_JUMP_IF_TRUE -> Just Absolute
        _ -> Nothing

isBranch :: InstructionWInd -> Bool
isBranch ins = 
    case getBranchType ins of 
        Just _ -> True
        Nothing -> False

isReturn :: InstructionWInd -> Bool
isReturn ins =
    case opc ins of 
        RETURN_VALUE -> True
        _ -> False


data InstructionType = Branch | Return | Other deriving (Show)
instructionSize = 2 

getInstructionType :: InstructionWInd -> InstructionType
getInstructionType ins = 
    if isBranch ins then Branch
    else if isReturn ins then Return
    else Other

getNextInstruction :: Index -> Int 
getNextInstruction ind = case ind of 
    Index ind -> ind + 1

getNextInstructionOffset :: Index -> Int
getNextInstructionOffset ind = getNextInstruction ind * instructionSize


jumpTarget :: InstructionWInd -> Maybe Int
jumpTarget ins = 
    case getBranchType ins of 
        Just Absolute -> case a ins of 
            Just (Arg a) -> Just ((fromIntegral :: Word8 -> Int) a)
            Nothing -> Nothing
        Just Relative -> case a ins of 
            Just (Arg a) -> Just ((getNextInstructionOffset (ind ins) + (fromIntegral :: Word8 -> Int) a * 2) `div` instructionSize)
            Nothing -> Nothing
        Nothing -> Nothing



getBlockStarts :: [InstructionWInd] -> Int -> Set Int -> Set Int
getBlockStarts [] instructionsLength starts = starts
getBlockStarts (i:is) instructionsLength starts = 
    case getInstructionType i of 
        Branch -> case jumpTarget i of 
            Just target -> getBlockStarts is instructionsLength (Set.insert target (Set.insert (getNextInstruction (ind i)) starts))
            Nothing -> Set.fromList []
        Return -> if getNextInstruction (ind i) < instructionsLength then getBlockStarts is instructionsLength (Set.insert (getNextInstruction (ind i)) starts) else getBlockStarts is instructionsLength starts
        Other -> getBlockStarts is instructionsLength starts

createBlocks :: [Int] -> Int -> Int -> [InstructionWInd] -> BlockMap -> BlockMap 
createBlocks [] _ _ _ blockMap = blockMap
createBlocks starts instructionsLength i instr blockMap =
    if i < length starts then
        let endIndex = if i+1 < length starts then starts !! (i+1) else instructionsLength 
            in let block_instr = BytecodeSlice{start = starts !! i, end = endIndex} in --i set instructions as empty for now, may need to change it to include the instructions
                let block = Block {blockId=i, blockSlice=block_instr} in 
                    let newBlockMap = Map.insert (starts !! i) block (case blockMap of BlockMap m -> m) in
                        createBlocks starts instructionsLength (i+1) instr (BlockMap newBlockMap)
    else
        blockMap
       
createBlockMap :: [InstructionWInd] -> BlockMap
createBlockMap instructions = 
    let blockStarts = getBlockStarts instructions (length instructions) (Set.fromList [0]) in 
        let starts = List.sort (Set.toList blockStarts) in 
            createBlocks starts (length instructions) 0 instructions (BlockMap Map.empty)

-- Create CFG from blockmap and instructions

getListFromMap :: BlockMap -> [Block]
getListFromMap blockmap = 
    case blockmap of 
        BlockMap m -> 
            let list = Map.toList m in
                map snd list


getNeighborsFromIns :: InstructionWInd -> Int -> [Int]    
getNeighborsFromIns ins instrLength = 
    let jumpTargets = case ind ins of Index i -> if i == (instrLength - 1) then [] else [i + 1] in 
        case getInstructionType ins of 
            Branch -> case jumpTarget ins of 
                Just target -> target : jumpTargets
                Nothing -> jumpTargets
            Return -> []
            Other -> jumpTargets

getNeighborFromBlock :: Block -> [InstructionWInd] -> [Int]
getNeighborFromBlock block ins = 
    case block of 
        Block id slice ->
            case slice of 
                BytecodeSlice start end -> 
                    getNeighborsFromIns (ins !! (end-1)) (length ins)

addNeighborsToCFG :: CFG -> Int -> [Int] -> CFG
addNeighborsToCFG cfg key neighbors =
    case cfg of 
        CFG m ->
            let oldNeighbors = case Map.lookup key m of 
                                    Just n -> n
                                    Nothing -> [] in 
                CFG (Map.insert key (neighbors ++ oldNeighbors) m)


--loop through every block, for each block we get the jump neighbor and also the next neighbor
getFullCFGRecurse :: [Block] -> [InstructionWInd] -> CFG -> CFG
getFullCFGRecurse blocks instrs cfg = 
    case blocks of 
        (b : bs) -> 
            case b of 
                Block id slice ->
                    case slice of 
                        BytecodeSlice start end ->
                            let neighbors = getNeighborFromBlock b instrs in 
                                getFullCFGRecurse bs instrs (addNeighborsToCFG cfg start neighbors)
        [] -> cfg
    
           
getFullCFG :: BlockMap -> [InstructionWInd] -> CFG
getFullCFG blockmap instrs = 
    let blockList = getListFromMap blockmap in 
        getFullCFGRecurse blockList instrs (CFG (Map.empty))


testGetBlockStarts :: Test
testGetBlockStarts =
  TestList
    [ "first" ~: Set.fromList [0,4,6] ~=? getBlockStarts simpleProgramInstructions (length simpleProgramInstructions) (Set.fromList [0])]

simpleProgram = "def decisions(x)\n    if x >= 0:\n        return 1\n    return -1"
simpleProgramInstructions = [
    InstructionWInd {opc = LOAD_CONST, ind = Index 0, a = Just (Arg 0)}, 
    InstructionWInd {opc = LOAD_FAST, ind = Index 1, a = Just (Arg 1)},
    InstructionWInd {opc = COMPARE_OP, ind = Index 2, a = Just (Arg 5)},
    InstructionWInd {opc = POP_JUMP_IF_FALSE, ind = Index 3, a = Just (Arg 12)}, 
    
    InstructionWInd {opc = LOAD_CONST, ind = Index 4, a = Just (Arg 2)},
    InstructionWInd {opc = RETURN_VALUE, ind = Index 5, a = Nothing},
    
    InstructionWInd {opc = LOAD_CONST, ind = Index 6, a = Just (Arg 3)},
    InstructionWInd {opc = RETURN_VALUE, ind = Index 7, a = Nothing}
    ]
simpleProgramInstructions2 = [
    Instruction {opcode = LOAD_CONST, arg = Just (Arg 0)},
    Instruction {opcode = STORE_NAME, arg = Just (Arg 0)},
    Instruction {opcode = LOAD_NAME, arg = Just (Arg 0)},
    Instruction {opcode = LOAD_CONST, arg = Just (Arg 0)},
    Instruction {opcode = COMPARE_OP, arg = Just (Arg 2)},
    Instruction {opcode = POP_JUMP_IF_FALSE, arg = Just (Arg 2)},
    Instruction {opcode = LOAD_CONST, arg = Just (Arg 0)}
    ,Instruction {opcode = STORE_NAME, arg = Just (Arg 1)}
    ,Instruction {opcode = JUMP_FORWARD, arg = Just (Arg 1)}]
    
addIndexToInstructions :: [Instruction] -> [InstructionWInd]
addIndexToInstructions instrs = 
    let 
        addIndexToInstructions' :: [Instruction] -> Int -> [InstructionWInd]
        addIndexToInstructions' [] _ = []
        addIndexToInstructions' (l:ls) ind = 
            (InstructionWInd {opc = opcode l, ind = Index ind, a = arg l}) : addIndexToInstructions' ls (ind + 1)
    in addIndexToInstructions' instrs 0

removeIndexFromInstructions :: [InstructionWInd] -> [Instruction]
removeIndexFromInstructions instrs = 
    let 
        removeIndexFromInstructions' :: [InstructionWInd] -> [Instruction]
        removeIndexFromInstructions' [] = []
        removeIndexFromInstructions' (l:ls) = 
            (Instruction {opcode = opc l, arg = a l}) : removeIndexFromInstructions' ls
    in removeIndexFromInstructions' instrs


testAddIndexToInstructions :: Test
testAddIndexToInstructions =
  TestList
    [ "first" ~: simpleProgramInstructions ~=? addIndexToInstructions simpleProgramInstructions2]