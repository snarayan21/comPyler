module LiveVariableAnalysis where

import GenerateCFG
import Types  

import Data.Word

import Data.Map (Map)--as Map (empty, toList, fromListWith)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')
import qualified Data.List as List

import Test.HUnit hiding (State)
import Test.QuickCheck



newtype InState = InState (Set Int) deriving (Show, Eq)
newtype OutState = OutState (Set Int) deriving (Show, Eq) 
newtype Defs = Defs (Set Int) deriving (Show, Eq)
newtype Refs = Refs (Set Int) deriving (Show, Eq)
newtype Processed = Processed Bool deriving (Show, Eq)

newtype BlockToState = BlockToState (Map Int (InState, OutState, Processed)) deriving (Show) --mapping block start to state values
newtype WorkListStack = WorkListStack [Block] deriving (Show) 

isStore :: InstructionWInd -> Bool 
isStore ins = opc ins == STORE_NAME

isRef :: InstructionWInd -> Bool 
isRef ins = opc ins == LOAD_NAME

addIndexToInstate :: InState -> Int -> InState
addIndexToInstate instate index = case instate of 
    InState s -> InState (Set.insert index s)

removeIndexFromInstate :: InState -> Int -> InState
removeIndexFromInstate instate index = case instate of 
    InState s -> InState (Set.delete index s)


isInRange :: Block -> InstructionWInd -> Bool
isInRange block ins = 
    case block of 
        Block id slice ->
            case slice of 
                BytecodeSlice start end -> 
                    let index = ind ins in
                        case index of 
                            Index i -> i >= start && i < end
                            _ -> False
    
--liveness calculations
getInstrFromSlice :: Block -> [InstructionWInd] -> [InstructionWInd] 
getInstrFromSlice block ins = 
    case block of 
        Block id slice ->
            case slice of 
                BytecodeSlice start end -> 
                    filter (isInRange block) ins

reverseList :: [a] -> [a]    
reverseList = foldl (flip (:)) []


--gets all instate of a individual block, implement in reverse order of instructions
calculateInstateWithinBlockRecurse :: Block -> [InstructionWInd] -> Int -> InState -> InState
calculateInstateWithinBlockRecurse block ins index instate = 
    case ins of 
        (i : is) -> if isStore i then 
                        let newInstate = case a i of 
                                            Just (Arg a) -> removeIndexFromInstate instate ((fromIntegral :: Word8 -> Int) a)
                                            Nothing -> instate in
                            calculateInstateWithinBlockRecurse block is (index + 1) newInstate
                    else if isRef i then 
                        let newInstate = case a i of 
                                            Just (Arg a) -> addIndexToInstate instate ((fromIntegral :: Word8 -> Int) a)
                                            Nothing -> instate in
                            calculateInstateWithinBlockRecurse block is (index + 1) newInstate
                    else 
                        calculateInstateWithinBlockRecurse block is (index + 1) instate
        [] -> instate

outStateToInstate :: OutState -> InState
outStateToInstate outstate = 
    case outstate of 
        OutState s -> InState s

calculateInstateWithinBlock :: OutState -> Block -> [InstructionWInd] -> InState 
calculateInstateWithinBlock outstate block ins = 
    let instrs = reverseList $ getInstrFromSlice block ins in 
        calculateInstateWithinBlockRecurse block instrs 0 (outStateToInstate outstate)


getBlocksWithNoNeighbors :: CFG -> [Int]
getBlocksWithNoNeighbors cfg = 
    case cfg of 
        CFG m -> 
            let list = Map.toList m in
                map fst (filter (\(k, v) -> v == []) list)

startBlockIndexesToBlocks :: [Int] -> BlockMap -> [Block]
startBlockIndexesToBlocks indexes blockmap = 
    case blockmap of 
        BlockMap m -> 
            let list = Map.toList m in
                map snd (filter (\(k, v) -> elem k indexes) list)

--gets initial worklist stack given the cfg 
getInitialWorkListStack :: CFG -> BlockMap -> WorkListStack
getInitialWorkListStack cfg blockmap = 
    let blocks = getBlocksWithNoNeighbors cfg in 
        let blockList = startBlockIndexesToBlocks blocks blockmap in 
            WorkListStack blockList

getStartEndFromBlock :: Block -> (Int, Int)
getStartEndFromBlock block = 
    case block of 
        Block id slice ->
            case slice of 
                BytecodeSlice start end -> (start, end)

getStartFromBlock :: Block -> Int
getStartFromBlock block = 
    case getStartEndFromBlock block of 
        (start, end) -> start

getEndFromBlock :: Block -> Int
getEndFromBlock block = 
    case getStartEndFromBlock block of 
        (start, end) -> end


getBlockFromStart :: Int -> BlockMap -> Block
getBlockFromStart start blockmap = 
    case blockmap of 
        BlockMap m -> 
            case Map.lookup start m of 
                Just block -> block

getTupleFromBlock :: Block -> BlockToState -> (InState, OutState, Processed)
getTupleFromBlock block blocktostate = 
    let blockStart = getStartFromBlock block in 
        case blocktostate of 
            BlockToState m -> 
                case Map.lookup blockStart m of 
                    Just (instate, outstate, processed) -> (instate, outstate, processed)
                    Nothing -> (InState (Set.empty), OutState (Set.empty), Processed False)

getInstateFromBlock :: Block -> BlockToState -> InState
getInstateFromBlock block blocktostate = case getTupleFromBlock block blocktostate of 
    (instate, outstate, processed) -> instate

getOutstateFromBlock :: Block -> BlockToState -> OutState
getOutstateFromBlock block blocktostate = case getTupleFromBlock block blocktostate of 
    (instate, outstate, processed) -> outstate

getProcessedFromBlock :: Block -> BlockToState -> Processed
getProcessedFromBlock block blocktostate = case getTupleFromBlock block blocktostate of 
    (instate, outstate, processed) -> processed
                    

getBlockNeighbors :: Block -> CFG -> [Int]
getBlockNeighbors block cfg = 
    let blockStart = getStartFromBlock block in 
        case cfg of 
            CFG m -> 
                case Map.lookup blockStart m of 
                    Just neighbors -> neighbors

combineStates :: [InState] -> OutState
combineStates states = 
    foldl (\acc x -> case x of 
                        InState s -> case acc of
                                      OutState set -> OutState (Set.union set s)) (OutState (Set.empty)) states


--Get outstate of a block from its neighbors's instates
getOutState :: Block -> BlockMap -> CFG -> BlockToState -> OutState
getOutState block blockmap cfg blocktostate = 
    let neighbors = getBlockNeighbors block cfg in 
        let neighborBlocks = map (\x -> getBlockFromStart x blockmap) neighbors in 
            let neighborInStates = map (\x -> getInstateFromBlock x blocktostate) neighborBlocks in 
                combineStates neighborInStates

--Get all blocks with edges pointing to the given block
getReverseNeighbors :: Block -> CFG -> [Int]
getReverseNeighbors block cfg = 
    case cfg of 
        CFG m -> 
            let list = Map.toList m in 
                map fst (filter (\(k, v) -> elem (getStartFromBlock block) v) list)


updateBlockToState :: Block -> InState -> OutState -> Processed -> BlockToState -> BlockToState
updateBlockToState block instate outstate processed blocktostate = 
    let blockStart = getStartFromBlock block in 
        case blocktostate of 
            BlockToState m -> 
                BlockToState (Map.insert blockStart (instate, outstate, processed) m)

addBlocksToWorkList :: BlockMap -> [Int] -> WorkListStack -> WorkListStack
addBlocksToWorkList blockMap blocks worklist = 
    let blockList = map (\x -> getBlockFromStart x blockMap) blocks in 
        case worklist of 
            WorkListStack l -> WorkListStack (l ++ blockList)

--calculates the block state given an initial worklist stack, the instructions, the cfg, and an initial blocktostate
calculateLivenessRecurse :: WorkListStack -> BlockMap -> [InstructionWInd] -> CFG -> BlockToState -> BlockToState
calculateLivenessRecurse worklist blockmap instrs cfg blocktostate = 
    case worklist of 
        WorkListStack (b : bs) ->
            let (instate, outstate, processed) = getTupleFromBlock b blocktostate in 
                let newOutState = getOutState b blockmap cfg blocktostate in 
                    let newInState = calculateInstateWithinBlock newOutState b instrs in 
                        if (instate == newInState && case processed of Processed p -> p ) then 
                            let newBlockToState = updateBlockToState b newInState newOutState (Processed True) blocktostate in 
                                calculateLivenessRecurse (WorkListStack bs) blockmap instrs cfg newBlockToState
                        else 
                            let newBlockToState = updateBlockToState b newInState newOutState (Processed True) blocktostate in 
                                let newWorkList = addBlocksToWorkList blockmap(getReverseNeighbors b cfg) worklist in 
                                    calculateLivenessRecurse newWorkList blockmap instrs cfg newBlockToState
        WorkListStack [] -> blocktostate

calculateLivenessFinal :: [InstructionWInd] -> BlockToState 
calculateLivenessFinal instrs = 
    let blockmap = createBlockMap instrs in 
        let cfg = getFullCFG blockmap instrs in 
            let worklist = getInitialWorkListStack cfg blockmap in 
                let blocktostate = BlockToState (Map.empty) in 
                    calculateLivenessRecurse worklist blockmap instrs cfg blocktostate
