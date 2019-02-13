
module Env (
  Env(..),
  empty,
  lookup,
  remove,
  extend,
  extends,
  merge,
  mergeEnvs,
  singleton,
  keys,
  fromList,
  toList,
  pretty,
  baseEnv
) where

import Prelude hiding (lookup)

import Syntax
import Type

import Data.Monoid
import Data.Foldable hiding (toList)
import qualified Data.Map as Map
import Pretty
import Operators

-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

newtype Env = TypeEnv { types :: Map.Map Name Scheme }
  deriving (Eq, Show)

instance Pretty Env where
  pretty (TypeEnv e) = mconcat . fmap showAssoc . Map.toList $ e
    where showAssoc (n, s) = n ++ " : "++ pretty s ++ "\n"

baseEnv :: Env
baseEnv = TypeEnv $ Map.fromList allOps

empty :: Env
empty = TypeEnv Map.empty

extend :: Env -> (Name, Scheme) -> Env
extend env (x, s) = env { types = Map.insert x s (types env) }

remove :: Env -> Name -> Env
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

extends :: Env -> [(Name, Scheme)] -> Env
extends env xs = env { types = Map.union (Map.fromList xs) (types env) }

lookup :: Name -> Env -> Maybe Scheme
lookup key (TypeEnv tys) = Map.lookup key tys

merge :: Env -> Env -> Env
merge (TypeEnv a) (TypeEnv b) = TypeEnv (Map.union a b)

mergeEnvs :: [Env] -> Env
mergeEnvs = foldl' merge empty

singleton :: Name -> Scheme -> Env
singleton x y = TypeEnv (Map.singleton x y)

keys :: Env -> [Name]
keys (TypeEnv env) = Map.keys env

fromList :: [(Name, Scheme)] -> Env
fromList xs = TypeEnv (Map.fromList xs)

toList :: Env -> [(Name, Scheme)]
toList (TypeEnv env) = Map.toList env

instance Monoid Env where
  mempty = empty
  mappend = merge
