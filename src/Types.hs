{-# LANGUAGE RecordWildCards #-}

module Types where 

import Data.Word (Word8, Word16, Word32)
import Data.Map (Map)
import Data.List (sort)
import qualified Data.Map as Map
import Writer (PyCodeObj)

data Opcode
    = POP_TOP --1
    | ROT_TWO --2
    | ROT_THREE --3
    | DUP_TOP --4
    | NOP --9

-- VARIABLE ASSIGNMENT AND STORAGE
    | LOAD_FAST --124
    | LOAD_CONST --100
    | LOAD_NAME --101
    | LOAD_GLOBAL --116
    | LOAD_DEREF --136
    | LOAD_ATTR --106
    | LOAD_METHOD --131
    | STORE_FAST --125
    | STORE_NAME --90
    | STORE_GLOBAL --97
    | STORE_LOCALS --69
    | STORE_DEREF --137
    | STORE_ATTR --95
    | STORE_SUBSCR --60
    | DELETE_FAST --126
    | DELETE_NAME --91
    | DELETE_GLOBAL --97
    | DELETE_DEREF --138
    | DELETE_ATTR --96
    | DELETE_SUBSCR --61  

-- ARITHMETIC AND LOGIC
    | BINARY_ADD --23
    | BINARY_SUBTRACT --24
    | BINARY_MULTIPLY --20
    | BINARY_TRUE_DIVIDE --27
    | BINARY_FLOOR_DIVIDE --26
    | BINARY_MODULO --22
    | BINARY_POWER --19
    | BINARY_LSHIFT --62
    | BINARY_RSHIFT --63
    | BINARY_AND --64
    | BINARY_XOR --65
    | BINARY_OR -- 66
    | UNARY_NOT --12
    | UNARY_POSITIVE --10
    | UNARY_NEGATIVE --11
    | COMPARE_OP --107

-- LOOPS, JUMPS
    | JUMP_FORWARD --110
    | JUMP_IF_FALSE_OR_POP --111
    | JUMP_IF_TRUE_OR_POP --112
    | JUMP_ABSOLUTE --113
    | POP_JUMP_IF_FALSE --114
    | POP_JUMP_IF_TRUE --115
    | BREAK_LOOP -- 80
    | CONTINUE_LOOP --119
    | SETUP_LOOP --120
    | GET_ITER --68
    | FOR_ITER --93
    | BINARY_SUBSCR --25

-- FUNCTIONS
    | MAKE_FUNCTION --132
    | MAKE_CLOSURE --134
    | LOAD_CLOSURE --135
    | CALL_FUNCTION --131
    | RETURN_VALUE --83
    | CALL_METHOD --161

-- DATA
    | BUILD_TUPLE --102
    | BUILD_LIST --103
    | BUILD_SET --104
    | BUILD_MAP --105
    | UNPACK_SEQUENCE --92
    | STORE_MAP --54

 -- MISC
    | PRINT_EXPR --70
    | POP_BLOCK --87

    deriving (Eq, Ord, Show)

opcodeList :: [(Opcode, Word8)]
opcodeList = [
    (POP_TOP, 1),
    (ROT_TWO, 2),
    (ROT_THREE, 3),
    (DUP_TOP, 4),
    (NOP, 9),
    (BINARY_SUBSCR, 25),
    (UNPACK_SEQUENCE, 92),
    (STORE_MAP, 54),
    (LOAD_FAST, 124),
    (LOAD_CONST, 100),
    (LOAD_NAME, 101),
    (LOAD_GLOBAL, 116),
    (LOAD_DEREF, 136),
    (LOAD_ATTR, 106),
    (LOAD_METHOD, 160),
    (CALL_METHOD, 161),
    (STORE_FAST, 125),
    (STORE_NAME, 90),
    (STORE_GLOBAL, 97),
    (STORE_LOCALS, 69),
    (STORE_DEREF, 137),
    (STORE_ATTR, 95),
    (STORE_SUBSCR, 60),
    (DELETE_FAST, 126),
    (DELETE_NAME, 91),
    (DELETE_GLOBAL, 97),
    (DELETE_DEREF, 138),
    (DELETE_ATTR, 96),
    (DELETE_SUBSCR, 61),
    (BINARY_ADD, 23),
    (BINARY_SUBTRACT, 24),
    (BINARY_MULTIPLY, 20),
    (BINARY_TRUE_DIVIDE, 27),
    (BINARY_FLOOR_DIVIDE, 26),
    (BINARY_MODULO, 22),
    (BINARY_POWER, 19),
    (BINARY_LSHIFT, 62),
    (BINARY_RSHIFT, 63),
    (BINARY_AND, 64),
    (BINARY_XOR, 65),
    (BINARY_OR,  66),
    (UNARY_NOT, 12),
    (UNARY_POSITIVE, 10),
    (UNARY_NEGATIVE, 11),
    (COMPARE_OP, 107),
    (JUMP_FORWARD, 110),
    (JUMP_IF_FALSE_OR_POP, 111),
    (JUMP_IF_TRUE_OR_POP, 112),
    (JUMP_ABSOLUTE, 113),
    (POP_JUMP_IF_FALSE, 114),
    (POP_JUMP_IF_TRUE, 115),
    (BREAK_LOOP,  80),
    (CONTINUE_LOOP, 119),
    (SETUP_LOOP, 120),
    (MAKE_FUNCTION, 132),
    (MAKE_CLOSURE, 134),
    (LOAD_CLOSURE, 135),
    (CALL_FUNCTION, 131),
    (RETURN_VALUE, 83),
    (BUILD_TUPLE, 102),
    (BUILD_LIST, 103),
    (BUILD_SET, 104),
    (BUILD_MAP, 105),
    (PRINT_EXPR, 70),
    (POP_BLOCK, 87),
    (GET_ITER, 68),
    (FOR_ITER, 93)
    ]

data CallArgs =
   CallArgs
   { callArgs_pos :: Word8
   , callArgs_keyword :: Word8
   }

initCallArgs :: CallArgs
initCallArgs =
   CallArgs
   { callArgs_pos = 0
   , callArgs_keyword = 0
   }

data VarContext = 
    Module
    | Function deriving (Show)

newtype Index = Index Int deriving (Show, Eq, Ord)
newtype Arg = Arg Word8 deriving (Show, Eq, Ord)

--one bytecode instruction has opcode and (optional) arg
data Instruction = 
    Instruction {
        opcode :: Opcode,
        arg :: Maybe Arg
    } deriving (Show, Eq)

data InstructionWInd = 
    InstructionWInd {
        opc :: Opcode,
        ind :: Index,
        a :: Maybe Arg
    } deriving (Show, Eq)

--annotated code

--returns length of bytecode instruction, in bytes
bytecodeLen :: Instruction -> Int
bytecodeLen (Instruction {..}) = 2 --in python 3.6+, all instructions are 2 bytes. Both opcode and arg are Word8.

-- data VarType = CellVar Int | Local | Global | Free Int | Cell Int | Unknown
data OpcodeType = Fast | Deref | Name | Global | Unknown deriving (Show)
-- change string to Identifier type
type Identifier = String
type VarMap = Map Identifier Arg
type PyObjMap = Map PyCodeObj Arg
type LocMap = Map Int [Int]
--type VarSet = Set Identifier

data OpcodeVariant = Load | Store | Delete | UnknownVariant

data CodeState = 
    CodeState {
        instructions :: [Instruction],
        nameIndex :: Int,
        stateLabel :: Int,
        cellVars :: VarMap,
        freeVars :: VarMap,  
        fastVars :: VarMap,
        globalVars :: VarMap,
        namesMap :: VarMap,
        namesArr :: [Identifier],
        constantsMap :: PyObjMap,
        constantsArr :: [PyCodeObj],
        varContext :: VarContext,
        flags :: Word32,
        linenumtable :: [(Word16, Word32)], --bytecode offset to source line number mapping, stored with end correspondence in beginning of list
        filename :: String,
        objname :: String,
        conditional_locs :: LocMap,
        nestlevel :: Int,
        forNestLevel :: Int,
        for_locs :: LocMap,
        isInDot :: Bool
    } deriving (Show)

initCodeState :: String -> CodeState
initCodeState fname = CodeState {
    instructions = [],
    nameIndex = 0,
    stateLabel = 0,
    cellVars = Map.empty,
    freeVars = Map.empty,
    fastVars = Map.empty,
    globalVars = Map.empty,
    namesMap = Map.empty,
    namesArr = [],
    constantsMap = Map.empty,
    constantsArr = [],
    varContext = Types.Module,
    flags = 0,
    linenumtable = [],
    filename = "",
    objname = "",
    conditional_locs = Map.fromList [(0,[])],
    for_locs = Map.fromList [(0,[])],
    nestlevel = -1,
    forNestLevel = -1,
    isInDot = False
}

--return identifiers sorted by their associated indices
getSortedIdents :: VarMap -> [Identifier]
getSortedIdents vmap = map snd (sort [(idx, ident) | (ident, idx) <- Map.assocs vmap])

