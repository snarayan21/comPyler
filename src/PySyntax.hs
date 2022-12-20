{-# LANGUAGE OverloadedStrings #-}

module PySyntax (quickCheckN, genVar2, genExp2, genStatement2, genBlock2, genStatementAssign, genAssignments, genPrints, genProgram, genFilename, varnames, sampleBlock2, sampleAssignments, samplePrints, samplePrograms, sampleSingleProgram, getSingleProgram, writeSingleProgram, prop_exec_same, testPyGen, testPyGenN, pretty, oneLine) where

import Control.Monad (when)
import Data.Map (Map)
import qualified Data.Map as Map
import Test.QuickCheck (Arbitrary (..), Gen)
import qualified Test.QuickCheck.Monadic as QCM (assert, monadicIO, run)
import qualified Test.QuickCheck as QC
import Text.PrettyPrint (Doc, (<+>))
import qualified Text.PrettyPrint as PP
import System.Process (callCommand)
import Main (compileToPyc)

newtype Block = Block [Statement] -- s1 ... sn
  deriving (Eq, Show)

newtype Assignments = Assignments [Statement] deriving (Eq, Show)

newtype Prints = Prints [Var] deriving (Eq, Show)

data Program = Program Assignments Block Prints deriving (Eq, Show)

instance Semigroup Block where
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty = Block []

data Statement
  = Assign Var Expression -- x = e
  | If Expression Block Block -- if e then s1 else s2
  | IfElif Expression Expression Block Block Block -- if e1 then s1 elif e2 then s2 else s3-}
  deriving (Eq, Show)

data Expression
  = Var Var -- global variables x and table indexing
  | Val Value -- literal values
  | Op1 Uop Expression -- unary operators
  | Op2 Expression Bop Expression -- binary operators
  deriving (Eq, Show)

data Value
  = IntVal Int -- 1
  | BoolVal Bool -- false, true --> NOTE: can result in division by zero errors!
  deriving (Eq, Show, Ord)

--Python int and bool should be closed under these operations :)

data Uop
  = Pos -- `+` :: Int -> Int
  | Neg -- `-` :: Int -> Int --> NOTE: unsupported for strings! works with bool in python.
  | Not -- `not` :: a -> Bool
  deriving (Eq, Show, Enum, Bounded)

data Bop
  = Plus -- `+`  :: Int -> Int -> Int  -- not supported by string. removing string.
  | Minus -- `-`  :: Int -> Int -> Int  -- not supported by string. removing string.
  | Times -- `*`  :: Int -> Int -> Int  -- not supported by string. removing string.
  | Eq -- `==` :: a -> a -> Bool
  | Neq -- '!=' :: a -> a -> Bool
  | Gt -- `>`  :: a -> a -> Bool
  | Ge -- `>=` :: a -> a -> Bool
  | Lt -- `<`  :: a -> a -> Bool
  | Le -- `<=` :: a -> a -> Bool
  deriving (Eq, Show, Enum, Bounded)

type Name = String -- either the name of a variable or the name of a field

newtype Var = Name Name deriving (Eq, Show)

class PP a where
  pp :: a -> Doc

-- | Default operation for the pretty printer. Displays using standard formatting
-- rules, with generous use of indentation and newlines.
pretty :: PP a => a -> String
pretty = PP.render . pp

-- | Compact version. Displays its argument without newlines.
oneLine :: PP a => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

instance PP Uop where
  pp :: Uop -> Doc
  pp Pos = PP.char '+'
  pp Neg = PP.char '-'
  pp Not = PP.text "not"

instance PP Bool where
  pp :: Bool -> Doc
  pp True = PP.text "True"
  pp False = PP.text "False"

instance PP String where
  pp :: String -> Doc
  pp = PP.text

instance PP Int where
  pp :: Int -> Doc
  pp = PP.int

instance PP Var where
  pp :: Var -> Doc
  pp (Name n) = PP.text n

instance PP Value where
  pp :: Value -> Doc
  pp (IntVal i) = pp i
  pp (BoolVal b) = pp b

isBase :: Expression -> Bool
isBase Val {} = True
isBase Var {} = True
isBase Op1 {} = True
isBase _ = False

instance PP Bop where
  pp :: Bop -> Doc
  pp Plus = PP.char '+'
  pp Minus = PP.char '-'
  pp Times = PP.char '*'
  pp Gt = PP.char '>'
  pp Ge = PP.text ">="
  pp Lt = PP.char '<'
  pp Le = PP.text "<="
  pp Eq = PP.text "=="
  pp Neq = PP.text "!="

instance PP Expression where
  pp :: Expression -> Doc
  pp (Var v) = pp v
  pp (Val v) = pp v
  pp (Op1 Pos v) = pp Pos <+> if isBase v then pp v else PP.parens (pp v)
  pp (Op1 Neg v) = pp Neg <+> if isBase v then pp v else PP.parens (pp v)
  pp (Op1 Not v) = PP.parens (pp Not <+> if isBase v then pp v else PP.parens (pp v))
  pp e@Op2 {} = ppPrec 0 e
    where
      ppPrec n (Op2 e1 bop e2) =
        ppParens (level bop < n) $
          ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2
      ppPrec _ e' = pp e'
      ppParens b = if b then PP.parens else id

instance PP Block where
  pp :: Block -> Doc
  pp (Block [s]) = pp s
  pp (Block ss) = PP.vcat (map pp ss)

instance PP Assignments where
  pp :: Assignments -> Doc
  pp (Assignments [a]) = pp a
  pp (Assignments aa) = PP.vcat (map pp aa)

instance PP Prints where
  pp :: Prints -> Doc
  pp (Prints [Name n]) = PP.text "print(" <+> PP.text n <+> PP.text ")"
  pp (Prints prpr) = PP.vcat (map specialp prpr) where
    specialp :: Var -> Doc
    specialp (Name na)= PP.text "print(" <+> PP.text na <+> PP.text ")"

instance PP Program where
  pp :: Program -> Doc
  pp (Program assigns block prints) = pp assigns PP.$$ pp block PP.$$ pp prints

--python should use 4 space indendation
instance PP Statement where
  pp (Assign x e) = pp x <+> PP.equals <+> pp e
  pp (If guard b1 b2) =
    (PP.text "if" <+> pp guard <+> PP.text ":")
      PP.$$ PP.nest 4 (pp b1)
    PP.$$ PP.text "else:"
      PP.$$ PP.nest 4 (pp b2)
  pp (IfElif guard1 guard2 b1 b2 b3) =
    (PP.text "if" <+> pp guard1 <+> PP.text ":")
      PP.$$ PP.nest 4 (pp b1)
    PP.$$ (PP.text "elif" <+> pp guard2 <+> PP.text ":")
      PP.$$ PP.nest 4 (pp b2)
    PP.$$ PP.text "else:"
      PP.$$ PP.nest 4 (pp b3)

level :: Bop -> Int
level Times = 7
level Plus = 5
level Minus = 5
level _ = 3 -- comparison operators

instance PP a => PP (Map Value a) where
  pp m = PP.braces (PP.vcat (map ppa (Map.toList m)))
    where
      ppa (v1, v2) = PP.brackets (pp v1) <+> PP.text "=" <+> pp v2

instance PP a => PP (Map Name a) where
  pp m = PP.braces (PP.vcat (map ppa (Map.toList m)))
    where
      ppa (s, v2) = PP.text s <+> PP.text "=" <+> pp v2

quickCheckN :: QC.Testable prop => prop -> Int -> IO ()
quickCheckN p n = QC.quickCheckWith (QC.stdArgs {QC.maxSuccess = n, QC.maxSize = 100}) p

-- select var from list of vars
genVar2 :: [Var] -> Gen Var
genVar2 = QC.elements

-- Generate expression, given list of variable names
genExp2 :: [Var] -> Int -> Gen Expression
genExp2 vnames 0 = QC.oneof [Var <$> genVar2 vnames, Val <$> arbitrary]
genExp2 vnames n =
  QC.frequency
    [ (1, Var <$> genVar2 vnames),
      (1, Val <$> arbitrary),
      -- unary operators half as often as binary.
      (n `div` 2, Op1 <$> arbitrary <*> genExp2 vnames n'),
      (n, Op2 <$> genExp2 vnames n' <*> arbitrary <*> genExp2 vnames n')
    ]
  where
    n' = n `div` 2

-- | Generate a size-controlled statement, given list of variable names
genStatement2 :: [Var] -> Int -> Gen Statement
genStatement2 vnames n | n <= 1 = QC.oneof [Assign <$> genVar2 vnames <*> genExp2 vnames 0]
genStatement2 vnames n =
  QC.frequency
    [ (1, Assign <$> genVar2 vnames <*> genExp2 vnames n'),
      (n, If <$> genExp2 vnames n' <*> genBlock2 vnames n' <*> genBlock2 vnames n'),
      (n, IfElif <$> genExp2 vnames n' <*> genExp2 vnames n' <*> genBlock2 vnames n' <*> genBlock2 vnames n' <*> genBlock2 vnames n')
    ]
  where
    n' = 3*(n `div` 4)

-- generate block, given list of variable names
genBlock2 :: [Var] -> Int -> Gen Block
genBlock2 vns num = Block <$> genStmts2 vns num
  where
    genStmts2 vnames 0 = (:) <$> genStatement2 vnames 1 <*> return []
    genStmts2 vnames n =
      QC.frequency
        [ (1, (:) <$> genStatement2 vnames 1 <*> return []),
          (n, (:) <$> genStatement2 vnames n' <*> genStmts2 vnames n')
        ]
      where
        n' = n `div` 2

genStatementAssign :: Var -> Gen Statement
genStatementAssign vname = Assign <$> QC.elements [vname] <*> (Val <$> arbitrary)

genAssignments :: [Var] -> Gen Assignments
genAssignments allvnames = Assignments <$> genAssign allvnames where
  genAssign [] = return []
  genAssign (vname:vnames) = (:) <$> genStatementAssign vname <*> genAssign vnames

genPrints :: [Var] -> Gen Prints
genPrints vnames = Prints <$> QC.shuffle vnames

genProgram :: Gen Program
genProgram = Program <$> (arbitrary :: Gen Assignments) <*> (arbitrary :: Gen Block) <*> (arbitrary :: Gen Prints)

genFilename :: Gen String
genFilename = QC.listOf1 (QC.elements ['a' .. 'z'])

--possible variable names are lowecase single letters
varnames :: [Var]
varnames = Name . (:[]) <$> ['a' .. 'z']

sampleBlock2 :: IO ()
sampleBlock2 = QC.sample' (arbitrary :: Gen Block) >>= mapM_ (print . pp)

sampleAssignments :: IO()
sampleAssignments = QC.sample' (arbitrary :: Gen Assignments) >>= mapM_ (print . pp)

samplePrints :: IO()
samplePrints = QC.sample' (arbitrary :: Gen Prints) >>= mapM_ (print . pp)

samplePrograms :: IO()
samplePrograms = QC.sample' (arbitrary :: Gen Program) >>= mapM_ (print . pp)

sampleSingleProgram :: IO()
sampleSingleProgram = QC.generate (arbitrary :: Gen Program) >>= print . pp

getSingleProgram :: IO [Char]
getSingleProgram = do
  prog <- QC.generate (arbitrary :: Gen Program)
  return (show $ pp prog)

writeSingleProgram :: [Char] -> IO()
writeSingleProgram filename = do 
  prog <- getSingleProgram
  when (not (null prog)) $
        writeFile ("./py_gen/" ++ filename ++ ".py") prog
        
prop_exec_same :: QC.Property
prop_exec_same = QCM.monadicIO pytest where
    pytest = do
        fname <- QCM.run (QC.generate genFilename) -- get random filename
        QCM.run $ writeSingleProgram fname -- generates python program
        QCM.run $ Main.compileToPyc ("py_gen/" ++ fname ++ ".py") -- uses our compiler to generate .pyc file from generated code -}
        pyTrueOut <- QCM.run $ callCommand ("python " ++ "py_gen/" ++ fname ++ ".py")
        pyCompOut <- QCM.run $ callCommand ("python " ++ "py_gen/" ++ fname ++ ".pyc")
        QCM.assert (pyTrueOut == pyCompOut)

testPyGen :: IO ()
testPyGen = do QC.quickCheck prop_exec_same

testPyGenN :: Int -> IO ()
testPyGenN n = do quickCheckN prop_exec_same n

instance Arbitrary Var where
  {- arbitrary = QC.sized genVar -}
  arbitrary = genVar2 varnames
  shrink (Name _) = []

instance Arbitrary Statement where
  arbitrary = QC.sized (genStatement2 varnames)
  shrink (Assign v e) =
    [Assign v' e | v' <- shrink v]
      ++ [Assign v e' | e' <- shrink e]
  shrink (If e b1 b2) =
    first b1 ++ first b2
      ++ [If e' b1 b2 | e' <- shrink e]
      ++ [If e b1' b2 | b1' <- shrink b1]
      ++ [If e b1 b2' | b2' <- shrink b2]
  shrink (IfElif e1 e2 b1 b2 b3) =
    first b1 ++ first b2 ++ first b3
      ++ [IfElif e1' e2 b1 b2 b3 | e1' <- shrink e1]
      ++ [IfElif e1 e2' b1 b2 b3 | e2' <- shrink e2]
      ++ [IfElif e1 e2 b1' b2 b3 | b1' <- shrink b1]
      ++ [IfElif e1 e2 b1 b2' b3 | b2' <- shrink b2]
      ++ [IfElif e1 e2 b1 b2 b3' | b3' <- shrink b3]

instance Arbitrary Assignments where
  arbitrary = genAssignments varnames

instance Arbitrary Prints where
  arbitrary = genPrints varnames

instance Arbitrary Program where
  arbitrary = genProgram

-- | access the first statement in a block, if one exists
first :: Block -> [Statement]
first (Block []) = []
first (Block (x : _)) = [x]

instance Arbitrary Block where
  arbitrary = QC.sized (genBlock2 varnames)
  shrink (Block ss) = [Block ss' | ss' <- shrink ss]

instance Arbitrary Expression where
  arbitrary = QC.sized (genExp2 varnames)

  shrink (Val v) = Val <$> shrink v
  shrink (Var v) = Var <$> shrink v
  shrink (Op1 o e) = e : [Op1 o e' | e' <- shrink e]
  shrink (Op2 e1 o e2) =
    [Op2 e1' o e2 | e1' <- shrink e1]
      ++ [Op2 e1 o e2' | e2' <- shrink e2]
      ++ [e1, e2]

instance Arbitrary Uop where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Bop where
  arbitrary = QC.arbitraryBoundedEnum

instance Arbitrary Value where
  arbitrary =
    QC.oneof
      [ IntVal <$> arbitrary,
        BoolVal <$> arbitrary
      ]

  shrink (IntVal n) = IntVal <$> shrink n
  shrink (BoolVal b) = BoolVal <$> shrink b