{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Infer where

import Type
import Env
import Syntax
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Monad.Identity
import Control.Arrow
import Subst
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub)
import Pretty


type Constraint = (Type, Type);

instance Pretty Constraint where
  pretty (a, b) = pretty a ++ " should unify with : " ++ pretty b ++ "\n"

type Constraints= [Constraint]

instance Pretty Constraints where
  pretty = fmap pretty >>> unwords

instance Substituable Constraint where
  apply s (a, b) = (apply s a, apply s b)
  ftv (a, b) = ftv a `Set.union` ftv b

data TypeError
  = UnboundVariable String
  | InfiniteType Type
  | UnificationFail Type Type
  | UnificationMismatch [Type] [Type] deriving (Show, Eq);

type Infer a = (ReaderT Env (StateT InferState (Except TypeError)) a)

newtype InferState = InferState {count :: Int}

initInfer :: InferState
initInfer = InferState { count = 0 }

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = fst $ runWriter $ runExceptT $ solver st
  where st = (nullSubst, cs)

runSolveLog :: [Constraint] -> Writer String (Either TypeError Subst)
runSolveLog cs = runExceptT $ solver st
  where st = (nullSubst, cs)


inferTopLog :: Env -> [(String, Expr)] -> Writer String (Either TypeError Env)
inferTopLog env [] = return $ Right env
inferTopLog env ((name, ex):xs) = do
  e <- inferExprLog env ex
  case e of
    Left err -> return $ Left err
    Right ty -> do
      tell $ "found : " ++ pretty ty ++ "\n"
      inferTopLog (extend env (name, ty)) xs

inferTop :: Env -> [(String, Expr)] -> Either TypeError Env
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

inferExprLog :: Env -> Expr -> Writer String (Either TypeError Scheme)
inferExprLog env ex = case runInfer env (infer ex) of
  Left err -> return $ Left err
  Right (ty, cs) -> do
    tell $ "env : \n" ++ pretty env ++ "\n"
    tell $ pretty ty ++ "\n"
    tell $ "constraints : \n"++ pretty cs ++ "\n"
    sol <- runSolveLog cs
    case sol of
      Left err -> return $ Left err
      Right subst -> do
        tell $ "found subst : " ++ pretty subst
        return $ Right $ closeOver $ apply subst ty

inferExpr :: Env -> Expr -> Either TypeError Scheme
inferExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ apply subst ty

runInfer :: Env -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env m = runIdentity $ runExceptT $ evalStateT (runReaderT m env) initInfer

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalise Env.empty


normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (map TV letters)

    fv (TVar a)   = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TCon _)    = []

    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case Prelude.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

inEnv :: (Name, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

instantiate :: Scheme -> Infer Type -- replace by fresh variables
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalise :: Env -> Type -> Scheme
generalise env t = Forall as t
  where as = Set.toList $ ftv env `Set.difference` ftv t

fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

lookupEnv :: Name -> Infer Type
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s -> instantiate s

infer :: Expr -> Infer (Type, [Constraint])
infer = \case
  Lit (LInt _) -> return (typeInt, [])
  Lit (LBool _) -> return (typeBool, [])

  Var x -> do
      t <- lookupEnv x
      return (t, [])

  Lam x e -> do
    tv <- fresh
    (t, c) <- inEnv (x, Forall [] tv) (infer e)
    return (tv `TArr` t, c)

  App e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    return (tv, c1 ++ c2 ++ [(t1, t2 `TArr` tv)])

  Let x e1 e2 -> do
    env <- ask
    (t1, c1) <- infer e1
    case runSolve c1 of
      Left e -> throwError e
      Right sub -> do
        let sc = generalise (apply sub env) (apply sub t1)
        (t2, c2) <- inEnv (x, sc) $ local (apply sub) (infer e2)
        return (t2, c1 ++ c2)

  Fix e1 -> do
    (t1, c1) <- infer e1
    tv <- fresh
    return (tv, c1 ++ [(tv `TArr` tv, t1)])

type Unifier = (Subst, [Constraint])

type Solve a = ExceptT TypeError (Writer String) a

emptyUnifier :: Unifier
emptyUnifier = (nullSubst, [])

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return nullSubst
unifies (TVar v) t = bind v t
unifies t (TVar v) = bind v t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return nullSubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  s1 <- unifies t1 t2
  s2 <- unifyMany (apply s1 ts1) (apply s1 ts2)
  return $ s2 `compose` s1
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2): cs1) -> do
      tell $ "current su : " ++ pretty su ++ "\n"
      tell $ pretty t1 ++ " <=> " ++ pretty t2 ++ "\n\n"
      su1 <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs1)

-- tries to unify a and t
bind :: TVar -> Type -> Solve Subst
bind a t | t == TVar a = return nullSubst -- bind a t =
         | occursCheck a t = throwError $ InfiniteType t
         | otherwise = return $ Map.singleton a t

occursCheck :: Substituable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t
