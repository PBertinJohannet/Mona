{-# LANGUAGE LambdaCase #-}

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
  baseEnv,
  modifyClass,
  ClassEnv(..),
  baseClasses,
  DeclErr,
  runTypeDecls,
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
import Control.Arrow
import Pretty
import Operators
import Data.List (uncons)
import Data.Maybe (catMaybes)

-------------------------------------------------------------------------------
-- Typing Environment
-------------------------------------------------------------------------------

newtype ClassEnv = ClassEnv {classes :: Map.Map Name Class} deriving (Eq, Show)

newtype Env = TypeEnv { types :: Map.Map Name Scheme } deriving (Eq, Show)

instance Pretty Env where
  pretty (TypeEnv t) = mconcat . fmap showAssoc . Map.toList $ t
    where showAssoc (n, s) = n ++ " : "++ pretty s ++ "\n"

modifyClass :: ClassEnv -> (String, Class) -> ClassEnv
modifyClass env (n, c) = env{classes = Map.insert n c (classes env)}

baseEnv :: Env
baseEnv = TypeEnv (Map.fromList allOps)

baseClasses :: ClassEnv
baseClasses = ClassEnv (Map.fromList allClasses)

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
  | UnknownClass String;

instance Pretty DeclErr where
  pretty = \case
    AlreadyDefined s -> "Error : Type is already defined : " ++ s
    UnknownClass s -> "Error : Class is unknown : " ++ s

type AddTypes a = StateT Env (ExceptT DeclErr (Writer String)) a;

runTypeDecls :: Env -> [Decl] -> ExceptT DeclErr (Writer String) ([ExprDecl], Env)
runTypeDecls env = addTypeDecls >>> flip runStateT env

addTypeDecls :: [Decl] -> AddTypes [ExprDecl]
addTypeDecls ds = do
  l <- mapM addTypeDecl ds
  return $ mconcat l

addTypeDecl :: Decl -> AddTypes [ExprDecl]
addTypeDecl = \case
  (s, Expr e) -> return [(s, e)]
  (s, TypeDecl tvars (Or prods)) -> do
    addType s tvars prods
    return []

addType :: String -> [String] -> [Prod] -> AddTypes ()
addType name tvars pd = do
  env <- get
  case lookup name env of
    Just _ -> throwError $ AlreadyDefined name
    Nothing -> do
      cons <- mapM (constructor name tvars) pd
      modify $ flip extends cons
      return ()

constructor :: String -> [String] -> Prod -> AddTypes (String, Scheme)
constructor name tvars p@(Prod cname fs) =
  return (cname,
    Forall ((`TV` Star) <$> tvars) $ Qual [] $ inConstructor name tvars p)

inConstructor :: String -> [String] -> Prod -> Type
inConstructor name tvars (Prod n fs) =
  foldr mkArr
    (mkType name tvars)
    $ mkField name <$> fs

mkField :: String -> Field -> Type
mkField name = \case
  FieldS s -> (TCon s Star)
  FieldApp f fs -> foldl TApp (mkField name f) $ fmap (mkField name) fs

mkType :: String -> [String] -> Type
mkType s tvars = foldl TApp (TCon s Star) $ fmap (`TCon` Star) tvars
