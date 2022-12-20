module DeadcodeElim where

import GenerateCFG
import LiveVariableAnalysis
import Types  

import Data.Word

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')
import qualified Data.List as List

import Test.HUnit hiding (State)
import Test.QuickCheck

getBlockFromIndex :: Int -> BlockMap -> Block
getBlockFromIndex ind blockmap = 
    case blockmap of 
        BlockMap m -> 
            let list = Map.toList m in 
                let (k, v) = head (filter (\(k, v) -> ind >= k && ind < getEndFromBlock v) list) in 
                    v

-- Gets the block an instruction is part of 
getBlockFromIns :: InstructionWInd -> BlockMap -> Block 
getBlockFromIns (InstructionWInd _ index arg) blockmap = 
    case index of 
        Index i -> 
            let blockStart = getStartFromBlock (getBlockFromIndex i blockmap) in 
                let blockEnd = getEndFromBlock (getBlockFromIndex i blockmap) in 
                    if (i >= blockStart && i < blockEnd) then 
                        getBlockFromIndex i blockmap
                    else 
                        getBlockFromIndex (i - 1) blockmap


checkExistReferenceRecurse :: [InstructionWInd] -> Block -> Index -> Arg -> Bool
checkExistReferenceRecurse instrs block index arg = 
        case instrs of 
            (l : ls) -> 
                if isRef l then 
                    case l of 
                        InstructionWInd _ i a ->
                            if (i > index && case a of Just a -> a == arg) then True
                            else checkExistReferenceRecurse ls block index arg
                else 
                    checkExistReferenceRecurse ls block index arg
            [] -> False

checkExistReference :: [InstructionWInd] -> Block -> Index -> Arg -> Bool
checkExistReference instrs block index arg = 
    let instrSlice = getInstrFromSlice block instrs in 
        checkExistReferenceRecurse instrSlice block index arg


existsInList :: Int -> [Int] -> Bool
existsInList i l = 
    case l of 
        (x : xs) -> 
            if i == x then True
            else existsInList i xs
        [] -> False
                        
checkExists :: Int -> OutState -> Bool
checkExists i outstate = 
    case outstate of 
        OutState l -> 
            existsInList i (Set.toList l)


getDeadCodeLines :: BlockMap -> [InstructionWInd] -> BlockToState -> [Int]
getDeadCodeLines blockMap instrs blocktostate = 
    case instrs of 
        (l : ls) -> 
            if isStore l then 
                case l of 
                    InstructionWInd _ index arg -> 
                        let block = getBlockFromIns l blockMap in 
                            let (instate, outstate, processed) = getTupleFromBlock block blocktostate in 
                                case arg of 
                                    Just a -> 
                                        case a of 
                                            Arg aWord ->
                                                if not (checkExistReference ls block index a) then 
                                                    if not (checkExists ((fromIntegral :: Word8 -> Int) aWord) outstate) then 
                                                        case index of 
                                                            Index i -> i : getDeadCodeLines blockMap ls blocktostate
                                                    else 
                                                        getDeadCodeLines blockMap ls blocktostate
                                                else 
                                                    getDeadCodeLines blockMap ls blocktostate
                                    Nothing -> getDeadCodeLines blockMap ls blocktostate
            else 
                getDeadCodeLines blockMap ls blocktostate
        [] -> []


removeDeadCodeLines :: [InstructionWInd] -> [Int] -> [InstructionWInd]
removeDeadCodeLines instrs deadCodeLines = 
    case instrs of 
        (l : ls) -> 
            case l of 
                InstructionWInd _ index arg -> 
                    if existsInList (case index of Index i->i ) deadCodeLines then 
                        removeDeadCodeLines ls deadCodeLines
                    else 
                        l : removeDeadCodeLines ls deadCodeLines
        [] -> []


{-
x = 4
y = 5 
if x == 4: 
    z = 4
    y = 6
    print(y) 
else: 
    print ("GOODBYE")
    
print(z)
-}

instrTest1 = [Instruction {opcode = LOAD_CONST, arg = Just (Arg 0)},Instruction {opcode = STORE_NAME, arg = Just (Arg 0)},Instruction {opcode = LOAD_CONST, arg = Just (Arg 1)},Instruction {opcode = STORE_NAME, arg = Just (Arg 1)},Instruction {opcode = LOAD_NAME, arg = Just (Arg 0)},Instruction {opcode = LOAD_CONST, arg = Just (Arg 0)},Instruction {opcode = COMPARE_OP, arg = Just (Arg 2)},Instruction {opcode = POP_JUMP_IF_FALSE, arg = Just (Arg 17)},Instruction {opcode = LOAD_CONST, arg = Just (Arg 0)},Instruction {opcode = STORE_NAME, arg = Just (Arg 2)},Instruction {opcode = LOAD_CONST, arg = Just (Arg 2)},Instruction {opcode = STORE_NAME, arg = Just (Arg 1)},Instruction {opcode = LOAD_NAME, arg = Just (Arg 3)},Instruction {opcode = LOAD_NAME, arg = Just (Arg 1)},Instruction {opcode = CALL_FUNCTION, arg = Just (Arg 1)},Instruction {opcode = POP_TOP, arg = Nothing},Instruction {opcode = JUMP_FORWARD, arg = Just (Arg 4)},Instruction {opcode = LOAD_NAME, arg = Just (Arg 3)},Instruction {opcode = LOAD_CONST, arg = Just (Arg 3)},Instruction {opcode = CALL_FUNCTION, arg = Just (Arg 1)},Instruction {opcode = POP_TOP, arg = Nothing},Instruction {opcode = LOAD_NAME, arg = Just (Arg 3)},Instruction {opcode = LOAD_NAME, arg = Just (Arg 2)},Instruction {opcode = CALL_FUNCTION, arg = Just (Arg 1)},Instruction {opcode = POP_TOP, arg = Nothing},Instruction {opcode = LOAD_CONST, arg = Just (Arg 4)},Instruction {opcode = RETURN_VALUE, arg = Nothing}]
instrTest1WIndex = addIndexToInstructions instrTest1
testGetBlockMap1 = createBlockMap instrTest1WIndex
testGetCFG1 = getFullCFG testGetBlockMap1 instrTest1WIndex
liveness1 = calculateLivenessFinal instrTest1WIndex
testRemoveDeadcodeLines1 = removeDeadCodeLines instrTest1WIndex (getDeadCodeLines testGetBlockMap1 instrTest1WIndex liveness1)
testDeadcodeLines1 = getDeadCodeLines testGetBlockMap1 instrTest1WIndex liveness1
expected1 = [InstructionWInd {opc = LOAD_CONST, ind = Index 0, a = Just (Arg 0)},InstructionWInd {opc = STORE_NAME, ind = Index 1, a = Just (Arg 0)},InstructionWInd {opc = LOAD_CONST, ind = Index 2, a = Just (Arg 1)},InstructionWInd {opc = LOAD_NAME, ind = Index 4, a = Just (Arg 0)},InstructionWInd {opc = LOAD_CONST, ind = Index 5, a = Just (Arg 0)},InstructionWInd {opc = COMPARE_OP, ind = Index 6, a = Just (Arg 2)},InstructionWInd {opc = POP_JUMP_IF_FALSE, ind = Index 7, a = Just (Arg 17)},InstructionWInd {opc = LOAD_CONST, ind = Index 8, a = Just (Arg 0)},InstructionWInd {opc = STORE_NAME, ind = Index 9, a = Just (Arg 2)},InstructionWInd {opc = LOAD_CONST, ind = Index 10, a = Just (Arg 2)},InstructionWInd {opc = STORE_NAME, ind = Index 11, a = Just (Arg 1)},InstructionWInd {opc = LOAD_NAME, ind = Index 12, a = Just (Arg 3)},InstructionWInd {opc = LOAD_NAME, ind = Index 13, a = Just (Arg 1)},InstructionWInd {opc = CALL_FUNCTION, ind = Index 14, a = Just (Arg 1)},InstructionWInd {opc = POP_TOP, ind = Index 15, a = Nothing},InstructionWInd {opc = JUMP_FORWARD, ind = Index 16, a = Just (Arg 4)},InstructionWInd {opc = LOAD_NAME, ind = Index 17, a = Just (Arg 3)},InstructionWInd {opc = LOAD_CONST, ind = Index 18, a = Just (Arg 3)},InstructionWInd {opc = CALL_FUNCTION, ind = Index 19, a = Just (Arg 1)},InstructionWInd {opc = POP_TOP, ind = Index 20, a = Nothing},InstructionWInd {opc = LOAD_NAME, ind = Index 21, a = Just (Arg 3)},InstructionWInd {opc = LOAD_NAME, ind = Index 22, a = Just (Arg 2)},InstructionWInd {opc = CALL_FUNCTION, ind = Index 23, a = Just (Arg 1)},InstructionWInd {opc = POP_TOP, ind = Index 24, a = Nothing},InstructionWInd {opc = LOAD_CONST, ind = Index 25, a = Just (Arg 4)},InstructionWInd {opc = RETURN_VALUE, ind = Index 26, a = Nothing}]

{- 
x = 3
y = 2
for i in range(x): 
    if i == 2: 
        print(x)
    else: 
        z = 3
print(y)
 -}

instrTest2 = [InstructionWInd {opc = LOAD_CONST, ind = Index 0, a = Just (Arg 0)},InstructionWInd {opc = STORE_NAME, ind = Index 1, a = Just (Arg 0)},InstructionWInd {opc = LOAD_CONST, ind = Index 2, a = Just (Arg 1)},InstructionWInd {opc = STORE_NAME, ind = Index 3, a = Just (Arg 1)},InstructionWInd {opc = LOAD_NAME, ind = Index 4, a = Just (Arg 2)},InstructionWInd {opc = LOAD_NAME, ind = Index 5, a = Just (Arg 0)},InstructionWInd {opc = CALL_FUNCTION, ind = Index 6, a = Just (Arg 1)},InstructionWInd {opc = GET_ITER, ind = Index 7, a = Just (Arg 1)},InstructionWInd {opc = FOR_ITER, ind = Index 8, a = Just (Arg 14)},InstructionWInd {opc = STORE_NAME, ind = Index 9, a = Just (Arg 3)},InstructionWInd {opc = LOAD_NAME, ind = Index 10, a = Just (Arg 3)},InstructionWInd {opc = LOAD_CONST, ind = Index 11, a = Just (Arg 1)},InstructionWInd {opc = COMPARE_OP, ind = Index 12, a = Just (Arg 2)},InstructionWInd {opc = POP_JUMP_IF_FALSE, ind = Index 13, a = Just (Arg 20)},InstructionWInd {opc = LOAD_NAME, ind = Index 14, a = Just (Arg 4)},InstructionWInd {opc = LOAD_NAME, ind = Index 15, a = Just (Arg 0)},InstructionWInd {opc = CALL_FUNCTION, ind = Index 16, a = Just (Arg 1)},InstructionWInd {opc = POP_TOP, ind = Index 17, a = Nothing},InstructionWInd {opc = LOAD_CONST, ind = Index 18, a = Just (Arg 2)},InstructionWInd {opc = RETURN_VALUE, ind = Index 19, a = Nothing},InstructionWInd {opc = LOAD_CONST, ind = Index 20, a = Just (Arg 0)},InstructionWInd {opc = STORE_NAME, ind = Index 21, a = Just (Arg 5)},InstructionWInd {opc = JUMP_ABSOLUTE, ind = Index 22, a = Just (Arg 8)},InstructionWInd {opc = LOAD_NAME, ind = Index 23, a = Just (Arg 4)},InstructionWInd {opc = LOAD_NAME, ind = Index 24, a = Just (Arg 1)},InstructionWInd {opc = CALL_FUNCTION, ind = Index 25, a = Just (Arg 1)},InstructionWInd {opc = POP_TOP, ind = Index 26, a = Nothing},InstructionWInd {opc = LOAD_CONST, ind = Index 27, a = Just (Arg 2)},InstructionWInd {opc = RETURN_VALUE, ind = Index 28, a = Nothing}]
testGetBlockMap2 = createBlockMap instrTest2
testGetCFG2 = getFullCFG testGetBlockMap2 instrTest2
liveness2 = calculateLivenessFinal instrTest2
testRemoveDeadcodeLines2 = removeDeadCodeLines instrTest2 (getDeadCodeLines testGetBlockMap2 instrTest2 liveness2)
expected2 = [InstructionWInd {opc = LOAD_CONST, ind = Index 0, a = Just (Arg 0)},InstructionWInd {opc = STORE_NAME, ind = Index 1, a = Just (Arg 0)},InstructionWInd {opc = LOAD_CONST, ind = Index 2, a = Just (Arg 1)},InstructionWInd {opc = STORE_NAME, ind = Index 3, a = Just (Arg 1)},InstructionWInd {opc = LOAD_NAME, ind = Index 4, a = Just (Arg 2)},InstructionWInd {opc = LOAD_NAME, ind = Index 5, a = Just (Arg 0)},InstructionWInd {opc = CALL_FUNCTION, ind = Index 6, a = Just (Arg 1)},InstructionWInd {opc = GET_ITER, ind = Index 7, a = Just (Arg 1)},InstructionWInd {opc = FOR_ITER, ind = Index 8, a = Just (Arg 14)},InstructionWInd {opc = STORE_NAME, ind = Index 9, a = Just (Arg 3)},InstructionWInd {opc = LOAD_NAME, ind = Index 10, a = Just (Arg 3)},InstructionWInd {opc = LOAD_CONST, ind = Index 11, a = Just (Arg 1)},InstructionWInd {opc = COMPARE_OP, ind = Index 12, a = Just (Arg 2)},InstructionWInd {opc = POP_JUMP_IF_FALSE, ind = Index 13, a = Just (Arg 20)},InstructionWInd {opc = LOAD_NAME, ind = Index 14, a = Just (Arg 4)},InstructionWInd {opc = LOAD_NAME, ind = Index 15, a = Just (Arg 0)},InstructionWInd {opc = CALL_FUNCTION, ind = Index 16, a = Just (Arg 1)},InstructionWInd {opc = POP_TOP, ind = Index 17, a = Nothing},InstructionWInd {opc = LOAD_CONST, ind = Index 18, a = Just (Arg 2)},InstructionWInd {opc = RETURN_VALUE, ind = Index 19, a = Nothing},InstructionWInd {opc = LOAD_CONST, ind = Index 20, a = Just (Arg 0)},InstructionWInd {opc = JUMP_ABSOLUTE, ind = Index 22, a = Just (Arg 8)},InstructionWInd {opc = LOAD_NAME, ind = Index 23, a = Just (Arg 4)},InstructionWInd {opc = LOAD_NAME, ind = Index 24, a = Just (Arg 1)},InstructionWInd {opc = CALL_FUNCTION, ind = Index 25, a = Just (Arg 1)},InstructionWInd {opc = POP_TOP, ind = Index 26, a = Nothing},InstructionWInd {opc = LOAD_CONST, ind = Index 27, a = Just (Arg 2)},InstructionWInd {opc = RETURN_VALUE, ind = Index 28, a = Nothing}]

testRemoveDeadLines1 :: Test
testRemoveDeadLines1 =
  TestList
    [ "first" ~: expected1 ~=? testRemoveDeadcodeLines1,
      "second" ~: expected2 ~=? testRemoveDeadcodeLines2
    ]