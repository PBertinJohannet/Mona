{-# LANGUAGE LambdaCase #-}

module Env (
  Env(..),
  Envs(..),
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
  alterClass,
  addClass,
  ClassEnv(..),
  DeclErr,
  baseEnvs,
  letters,
  addInstance,
  lettersSim,
) where

import Prelude hiding (lookup)

import Syntax
import Type

import Data.Monoid
import Data.Foldable hiding (toList)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Arrow
import Pretty
import Operators
import Data.List (uncons)
import Data.Maybe (catMaybes)

-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

letters :: [String]
letters = ('\'':) <$> lettersSim

lettersSim :: [String]
lettersSim = [1..] >>= flip replicateM ['a'..'z']

newtype ClassEnv = ClassEnv {classes :: Map.Map Name Class} deriving (Eq, Show)

newtype Env = TypeEnv { types :: Map.Map Name Scheme } deriving (Eq, Show)

data Envs = Envs{ dataEnv :: Env, varEnv :: Env, classEnv :: ClassEnv} deriving (Eq, Show)

instance Pretty Env where
  pretty (TypeEnv t) = mconcat . fmap showAssoc . Map.toList $ t
    where showAssoc (n, s) = n ++ " : "++ pretty s ++ "\n"

instance ShowKind Env where
  showKind (TypeEnv t) = mconcat . fmap showAssoc . Map.toList $ t
    where showAssoc (n, s) = n ++ " : "++ showKind s ++ "\n"

instance Pretty ClassEnv where
  pretty (ClassEnv t) = mconcat . fmap showAssoc . Map.toList $ t
    where showAssoc (n, s) = n ++ " : "++ pretty s ++ "\n"

instance Pretty Envs where
  pretty (Envs d v c) =
    "Vars : \n" ++ pretty v ++
    "\nTypes : \n" ++ pretty d ++
    "\nClasses : \n" ++ pretty c ++ "\n"

instance ShowKind Envs where
  showKind (Envs d v c) =
    "Vars : \n" ++ showKind v ++ "\nTypes : \n" ++ pretty d ++ "\n"

baseEnvs :: Envs
baseEnvs = Envs kindEnv baseEnv baseClasses

addClass :: ClassEnv -> (String, Class) -> ClassEnv
addClass env (n, c) = alterClass env n (Just . const c)

addInstance :: ClassEnv -> String -> Inst -> ClassEnv
addInstance env n i = modifyClass env n $ second (i:)

modifyClass :: ClassEnv -> String -> (Class -> Class) -> ClassEnv
modifyClass env n f = alterClass env n (fmap f)

alterClass :: ClassEnv -> String -> (Maybe Class -> Maybe Class) -> ClassEnv
alterClass env name f = env{classes = Map.alter f name (classes env)}

baseEnv :: Env
baseEnv = TypeEnv (Map.fromList allOps)

baseClasses :: ClassEnv
baseClasses = ClassEnv (Map.fromList allClasses)

kindEnv :: Env
kindEnv = TypeEnv (Map.fromList allKinds)

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

data DeclErr
  = AlreadyDefined String
  | UnknownClass String
  | EmptyType String;

instance Pretty DeclErr where
  pretty = \case
    AlreadyDefined s -> "Type is already defined : " ++ s
    UnknownClass s -> "Class is unknown : " ++ s
    EmptyType s -> "Empty type : " ++ s
