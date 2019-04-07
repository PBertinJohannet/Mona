module Env (
  TAst(..),
  Env(..),
  Envs(..),
  empty,
  lookup,
  remove,
  extend,
  extendAst,
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
  keysK,
  baseEnvs,
  letters,
  addInstance,
  withCompiled,
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
import Run
import Subst

-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

letters :: [String]
letters = ('\'':) <$> lettersSim

lettersSim :: [String]
lettersSim = [1..] >>= flip replicateM ['a'..'z']

newtype ClassEnv = ClassEnv {classes :: Map.Map Name Class} deriving (Eq, Show)

newtype Env = TypeEnv { types :: Map.Map Name Scheme } deriving (Eq, Show)

data TAst = TAst { texprs :: Map.Map Name TExpr, compiled :: Map.Map Name (Run Value)}

data Envs = Envs{ dataEnv :: Env, varEnv :: Env, classEnv :: ClassEnv, ast :: TAst}

keysK :: TAst -> String
keysK (TAst t c) = mconcat . fmap showAssoc . Map.toList $ t
  where showAssoc (n, _) = n  ++ "\n"

instance Pretty TAst where
  pretty (TAst t c) = mconcat . fmap showAssoc . Map.toList $ t
    where showAssoc (n, t) = n ++ " : "++ pretty t ++ "\n\n"

instance Pretty Env where
  pretty (TypeEnv t) = mconcat . fmap showAssoc . Map.toList $ t
    where showAssoc (n, s) = n ++ " : "++ pretty s ++ "\n"

instance ShowKind Env where
  showKind (TypeEnv t) = mconcat . fmap showAssoc . Map.toList $ t
    where showAssoc (n, s) = n ++ " : "++ showKind s ++ "\n"

instance Pretty ClassEnv where
  pretty (ClassEnv t) = mconcat . fmap showAssoc . Map.toList $ t
    where showAssoc (n, s) = n ++ " : "++ show s ++ "\n"

instance Pretty Envs where
  pretty (Envs d v c e) =
    "Vars : \n" ++ pretty v ++
    "\nTypes : \n" ++ pretty d ++
    "\nClasses : \n" ++ pretty c ++
    "\nsources : \n" ++ pretty e ++ "\n"

instance ShowKind Envs where
  showKind (Envs d v c s) =
    "Vars : \n" ++ showKind v ++ "\nTypes : \n" ++ pretty d ++
    "\nClasses : \n" ++ pretty c ++
    "\nsources : \n" ++ pretty s ++ "\n"

baseEnvs :: Envs
baseEnvs = Envs kindEnv baseEnv baseClasses baseSource

addClass :: ClassEnv -> (String, Class) -> ClassEnv
addClass env (n, c) = alterClass env n (Just . const c)

addInstance :: ClassEnv -> String -> Inst -> ClassEnv
addInstance env n i = modifyClass env n $ second (i:)

modifyClass :: ClassEnv -> String -> (Class -> Class) -> ClassEnv
modifyClass env n f = alterClass env n (fmap f)

alterClass :: ClassEnv -> String -> (Maybe Class -> Maybe Class) -> ClassEnv
alterClass env name f = env{classes = Map.alter f name (classes env)}

baseEnv :: Env
baseEnv = TypeEnv (Map.fromList allOpsTypes)

baseClasses :: ClassEnv
baseClasses = ClassEnv (Map.fromList allClasses)

kindEnv :: Env
kindEnv = TypeEnv (Map.fromList allKinds)

baseSource :: TAst
baseSource = TAst Map.empty Map.empty

withCompiled :: TAst -> [(Name, Run Value)] -> TAst
withCompiled env vals = env{compiled = Map.union (compiled env) (Map.fromList vals)}

empty :: Env
empty = TypeEnv Map.empty

extendAst :: TAst -> (Name, TExpr) -> TAst
extendAst env (x, s) = env { texprs = Map.insert x s (texprs env) }

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

instance Substituable Env where
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)
  ftv (TypeEnv env) = ftv (Map.elems env)

instance Substituable ClassEnv where
  apply s (ClassEnv c) = ClassEnv (Map.map (apply s) c)
  ftv (ClassEnv c) = ftv (Map.elems c)
