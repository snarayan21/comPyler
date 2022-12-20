{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Scope where

import Types
import Parser
import Control.Monad.State as S
import Data.Set as Set
import Language.Python.Common.AST as AST (
    ModuleSpan (..),
    Module (..),
    StatementSpan (..), 
    ParameterSpan (..),
    Statement (..),
    Expr (..),
    ExprSpan (..),
    Parameter (..),
    Ident (..),
    OpSpan (..),
    Op (..),
    Argument (..),
    ArgumentSpan(..))
import qualified Data.Map as Map
import Control.Monad.Reader (ReaderT, local, ask, runReaderT)

type IdentifierSet = Set Identifier
type IdentifierArr = [Identifier]
type ScopeIdentifier = (Int, Int, Int, Int)

--variable names are just strings
data ScopeState = 
    ScopeState {
        assigned :: IdentifierArr,
        referenced :: IdentifierSet,
        globals :: IdentifierSet
    } deriving (Show, Eq, Ord) 

instance Semigroup ScopeState where
    (<>) (ScopeState a1 r1 g1) (ScopeState a2 r2 g2) = ScopeState (a1 <> a2) (r1 <> r2) (g1 <> g2)

instance Monoid ScopeState where
    mempty = ScopeState mempty mempty mempty

class Scope a where 
    getUsage :: a -> ScopeState

instance Scope t => Scope [t] where 
    getUsage = foldMap getUsage

instance (Scope t1, Scope t2) => Scope (t1, t2) where 
    getUsage (a, b) = getUsage a <> getUsage b

instance Scope StatementSpan where 
    getUsage (Assign { .. }) =  getUsage assign_to 
    getUsage (For { .. }) = getUsage for_body <> getUsage for_targets
    getUsage (While { .. }) = getUsage while_body
    getUsage (Conditional { .. }) = getUsage cond_guards <> getUsage cond_else
    getUsage _ = mempty

instance Scope ExprSpan where 
    getUsage (Var { .. }) = mempty {
        referenced = Set.singleton $ ident_string var_ident
    }
    getUsage _ = mempty

instance Scope ParameterSpan where 
    getUsage (Param { .. }) = mempty {
        assigned = [ident_string param_name]
    } 

getScope :: Scope a => [a] -> ScopeState
getScope = getUsage

ioLift :: IO a -> StateT CodeState IO a
ioLift = liftIO

compileScope :: Scope a => [a] -> StateT CodeState IO ScopeState
compileScope stmts = do
    return $ getScope stmts