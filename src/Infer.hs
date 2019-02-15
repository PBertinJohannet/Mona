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


data Constraint = Union (Type, Type) | InClass (Qual Type); -- a == b

instance Pretty Constraint where
  pretty = \case
    Union (a, b) -> pretty a ++ " should unify with : " ++ pretty b ++ "\n"
    InClass q -> pretty q ++ "\n"

type Constraints = [Constraint]

instance Pretty Constraints where
  pretty = fmap pretty >>> unwords

instance Substituable Constraint where
  apply s = \case
    Union (a, b) -> Union (apply s a, apply s b)
    InClass q  -> InClass $ apply s q
  ftv (Union (a, b)) = ftv a `Set.union` ftv b
  ftv (InClass q) = ftv q

data TypeError
  = UnboundVariable String
  | InfiniteType Type
  | UnificationFail Type Type
  | UnificationMismatch [Type] [Type] deriving (Show, Eq);

type Infer a = (ReaderT Env (StateT InferState (Except TypeError)) a)

type Solve a = (ReaderT ClassEnv (WriterT String (Except TypeError)) a)

newtype InferState = InferState {count :: Int}

initInfer :: InferState
initInfer = InferState { count = 0 }

runSolve :: ClassEnv -> [Constraint] -> Either TypeError Subst
runSolve env cs = fmap fst
  $ runIdentity
  $ runExceptT
  $ runWriterT
  $ runReaderT (solver (nullSubst, cs)) env

runSolveLog :: ClassEnv -> [Constraint] -> WriterT String (Except TypeError) Subst
runSolveLog env cs = runReaderT (solver (nullSubst, cs)) env


inferTopLog :: ClassEnv -> Env -> [(String, Expr)] -> WriterT String (Except TypeError) Env
inferTopLog _ env [] = return env
inferTopLog cenv env ((name, ex):xs) = do
  tell $ "infering : " ++ pretty ex ++ "\n"
  e <- inferExprLog cenv env ex
  tell $ "found : " ++ pretty e ++ "\n"
  inferTopLog cenv (extend env (name, e)) xs

inferTop :: ClassEnv -> Env -> [(String, Expr)] -> Either TypeError Env
inferTop cenv env [] = Right env
inferTop cenv env ((name, ex):xs) = case inferExpr cenv env ex of
  Left err -> Left err
  Right ty -> inferTop cenv (extend env (name, ty)) xs

inferExprLog :: ClassEnv -> Env -> Expr -> WriterT String (Except TypeError) Scheme
inferExprLog cenv env ex = case runInfer env (infer ex) of
  Left err -> throwError err
  Right (ty, cs) -> do
    tell $ "env : \n" ++ pretty env ++ "\n"
    tell $ pretty ty ++ "\n"
    tell $ "constraints : \n"++ pretty cs ++ "\n"
    subst <- runSolveLog cenv cs
    tell $ "found subst : " ++ pretty subst
    return $ closeOver $ apply subst ty

inferExpr :: ClassEnv -> Env -> Expr -> Either TypeError Scheme
inferExpr cenv env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cenv cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ apply subst ty

runInfer :: Env -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env m = runIdentity $ runExceptT $ evalStateT (runReaderT m env) initInfer

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalise Env.empty

normalize :: Scheme -> Scheme
normalize (Forall _ (Qual q body)) = Forall (map snd ord) (Qual q $ normtype body)
  where
    ord = zip (nub $ fv body) (map var letters)

    fv (TVar a)   = [a]
    fv (TApp a b) = fv a ++ fv b
    fv (TCon _ _)    = []

    normtype (TApp a b) = TApp (normtype a) (normtype b)
    normtype (TCon a k)   = TCon a k
    normtype (TVar a)   =
      case Prelude.lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

inEnv :: (Name, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

instantiate :: Scheme -> Infer (Qual Type) -- replace by fresh variables
instantiate (Forall as (Qual preds t)) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ Qual (apply s preds) (apply s t)

generalise :: Env -> Type -> Scheme
generalise env t = Forall as $ Qual [] t
  where as = Set.toList $ ftv env `Set.difference` ftv t

fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ tvar (letters !! count s)

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

lookupEnv :: Name -> Infer (Qual Type)
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
    Just s -> instantiate s
    Nothing -> throwError $ UnboundVariable (show x)


infer :: Expr -> Infer (Type, [Constraint])
infer = \case
  Lit (LInt _) -> return (typeInt, [])
  Lit (LBool _) -> return (typeBool, [])

  Var x -> do
      (Qual p t) <- lookupEnv x
      return (t, [InClass $ Qual p t])

  Lam x e -> do
    tv <- fresh
    (t, c) <- inEnv (x, Forall [] (Qual [] tv)) (infer e)
    return (tv `mkArr` t, c)

  App e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    return (tv, c1 ++ c2 ++ [Union (t1, t2 `mkArr` tv)])

  Fix e1 -> do
    (t1, c1) <- infer e1
    tv <- fresh
    return (tv, c1 ++ [Union (tv `mkArr` tv, t1)])

type Unifier = (Subst, [Constraint])


emptyUnifier :: Unifier
emptyUnifier = (nullSubst, [])

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return nullSubst
unifies (TVar v) t = bind v t
unifies t (TVar v) = bind v t
unifies (TApp t1 t2) (TApp t3 t4) = unifyMany [t1, t2] [t3, t4]
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
    (Union (t1, t2) : cs1) -> do
      tell $ "current su : " ++ pretty su ++ "\n"
      tell $ pretty t1 ++ " <=> " ++ pretty t2 ++ "\n\n"
      su1 <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs1)
    (InClass (Qual [] t) : cs1) -> solver (su, cs1)



-- tries to unify a and t
bind :: TVar -> Type -> Solve Subst
bind a t | t == TVar a = return nullSubst -- bind a t =
         | occursCheck a t = throwError $ InfiniteType t
         | otherwise = return $ Map.singleton a t

occursCheck :: Substituable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

unifyPred :: Pred -> Pred -> Solve Subst
unifyPred (IsIn i t) (IsIn i' t') | i == i' = unifies t t'
