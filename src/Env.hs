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
  modifyClass,
  ClassEnv(..),
  DeclErr,
  baseEnvs,
  letters,
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
letters = [1..] >>= flip replicateM ['a'..'z']

newtype ClassEnv = ClassEnv {classes :: Map.Map Name Class} deriving (Eq, Show)

newtype Env = TypeEnv { types :: Map.Map Name Scheme } deriving (Eq, Show)

data Envs = Envs{ dataEnv :: Env, varEnv :: Env, classEnv :: ClassEnv} deriving (Eq, Show)

instance Pretty Env where
  pretty (TypeEnv t) = mconcat . fmap showAssoc . Map.toList $ t
    where showAssoc (n, s) = n ++ " : "++ pretty s ++ "\n"

instance Pretty ClassEnv where
  pretty (ClassEnv t) = mconcat . fmap showAssoc . Map.toList $ t
    where showAssoc (n, s) = n ++ " : "++ pretty s ++ "\n"

instance Pretty Envs where
  pretty (Envs d v c) =
    "Vars : \n" ++ pretty v ++ "\nTypes : \n" ++ pretty d ++ "\n"

baseEnvs :: Envs
baseEnvs = Envs kindEnv baseEnv baseClasses

modifyClass :: ClassEnv -> (String, Class) -> ClassEnv
modifyClass env (n, c) = env{classes = Map.insert n c (classes env)}

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
{-
type AddTypes a = StateT Env (ExceptT DeclErr (Writer String)) a;

addTVar :: [String] -> [String]
addTVar vs = vs ++ [lastVar vs]

lastVar :: [String] -> String
lastVar vs = case find (`notElem` vs) letters of
  Just l -> l

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
      pats <- mapM (pat name tvars) pd
      modify $ flip extends (cons ++ pats)
      return ()

pat :: String -> [String] -> Prod -> AddTypes (String, Scheme)
pat name tvars p@(Prod cname fs) =
  let finalVar = TVar $ TV (lastVar tvars) Star in
  let utilFunc = inConstructor name tvars p finalVar in
  let vars = (`TV` Star) <$> addTVar tvars in
  let baseType = mkType name tvars in
  let construct = utilFunc `mkArr` (baseType `mkArr` finalVar) in
  let finalType = Forall vars (Qual [] construct) in
  return ("~" ++ cname, finalType)

constructor :: String -> [String] -> Prod -> AddTypes (String, Scheme)
constructor name tvars p@(Prod cname fs) =
  return (cname,
    Forall ((`TV` Star) <$> tvars)
    $ Qual []
    $ inConstructor name tvars p (mkType name tvars))

inConstructor :: String -> [String] -> Prod -> Type -> Type
inConstructor name tvars (Prod n fs) end =
  foldr mkArr
    end
    $ mkField name tvars <$> fs

mkField :: String -> [String] -> Field -> Type
mkField name tvars = \case
  FieldS s -> makeVar s tvars
  FieldApp a b -> TApp (mkField name tvars a) (mkField name tvars b)

mkType :: String -> [String] -> Type
mkType s tvars = foldl TApp (makeVar s tvars) $ fmap (TVar . (`TV` Star)) tvars

makeVar :: String -> [String] -> Type
makeVar v tvars = if v `elem` tvars then TVar (TV v Star) else TCon v Star
{-
inferKind :: [String] -> TypeDecl -> AddTypes Kind
inferKind = _ok
-}
type Infer a = ReaderT String -- tvars
  (StateT KindEnv
  (ExceptT DeclErr
  (Writer String))) a;

type Union = (Kind, TVar);
type Constraints = (Type, [Union]);

infer :: TypeDecl -> Infer (Type , [Constraints])
infer t = do
  name <- ask
  case t of
    Or [] -> throwError $ EmptyType name
    Or (p:ps) -> do
      (tp, cs) <- infer p
      is <- mapM infer ps
      (tps, css) <- unzip is
      return (tp, fmap (, tp) tps)
-}
