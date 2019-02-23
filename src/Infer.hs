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
import Data.List (nub, find)
import Pretty


newtype Union = Union (Type, Type) deriving Show;

instance Pretty Union where
  pretty (Union (a, b)) = pretty a ++ " should unify with : " ++ pretty b ++ "\n"

type Constraints = ([Union], [Pred])

instance Pretty Constraints where
  pretty (a, b) = pretty a ++ " => " ++ pretty b ++ "\n"

union :: (Type, Type) -> Constraints
union u = ([Union u], [])

instance Substituable Union where
  apply s (Union (a, b)) = Union (apply s a, apply s b)
  ftv (Union (a, b)) = ftv a `Set.union` ftv b

data TypeError
  = UnboundVariable String
  | InfiniteType Type
  | NotInClass Pred
  | UnknownClass String
  | WrongKind String Kind
  | UnificationFail Type Type
  | UnknownCommand String
  | UnificationMismatch [Type] [Type] deriving (Show, Eq);

instance Pretty TypeError where
  pretty = \case
    UnboundVariable s -> "Variable not in scope : "++ s
    InfiniteType t -> "Cannot create infinite type : "++pretty t
    NotInClass (IsIn a b) -> pretty b ++ " is not in " ++ a
    UnknownCommand s -> "Unknown command : " ++ s
    UnknownClass s -> "Unknown class : " ++ s
    WrongKind s k -> s ++ " has kind : " ++ pretty k
    UnificationFail t t' -> "Cannot unify : " ++ pretty t ++ " with "++pretty t'
    UnificationMismatch t t' -> "Cannot unify : " ++ pretty t ++ " with "++pretty t'

type Infer a = (ReaderT Env (StateT InferState (Except TypeError)) a)

type Solve a = (ReaderT ClassEnv (ExceptT TypeError (Writer String)) a)

type CSolve a = (ReaderT (ClassEnv, Subst) (ExceptT TypeError (Writer String)) a)

newtype InferState = InferState {count :: Int}

initInfer :: InferState
initInfer = InferState { count = 0 }

runSolve :: ClassEnv -> Constraints -> ExceptT TypeError (Writer String) ([Pred], Subst)
runSolve env cs = runReaderT (solver cs) env

inferDecl :: Envs -> [(String, [String], Expr)] -> ExceptT TypeError (Writer String) Envs
inferDecl env [] = return env
inferDecl (Envs d ev cenv) ((name, _, ex):xs) = do
  e <- inferExpr cenv d ex
  inferDecl (Envs (extend d (name, e)) ev cenv) xs


inferTop :: Envs -> [(String, Expr)] -> ExceptT TypeError (Writer String) Envs
inferTop env [] = return env
inferTop (Envs d env cenv) ((name, ex):xs) = do
  tell $ "infering : " ++ pretty ex ++ "\n"
  e <- inferExpr cenv env ex
  inferTop (Envs d (extend env (name, e)) cenv) xs

inferExpr :: ClassEnv -> Env -> Expr -> ExceptT TypeError (Writer String) Scheme
inferExpr cenv env ex = case runInfer env (infer ex) of
  Left err -> throwError err
  Right (ty, cs) -> do
    (preds, subst) <- runSolve cenv cs
    return $ closeOver (apply subst ty) (apply subst preds)

inferExprT :: ClassEnv -> Env -> Expr -> Scheme -> ExceptT TypeError (Writer String) Scheme
inferExprT cenv env ex tp = case runInfer env (inferEq ex tp) of
  Left err -> throwError err
  Right (ty, cs) -> do
    (preds, subst) <- runSolve cenv cs
    return $ closeOver (apply subst ty) (apply subst preds)

runInfer :: Env -> Infer (Type, Constraints) -> Either TypeError (Type, Constraints)
runInfer env m = runIdentity $ runExceptT $ evalStateT (runReaderT m env) initInfer

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> [Pred] -> Scheme
closeOver t p = (normalize . generalise Env.empty p) t

normalize :: Scheme -> Scheme
normalize (Forall _ (Qual q body)) =
  Forall (map snd ord) (Qual (q >>= normpred) (normtype body))
  where
    ord = zip (nub $ fv body) (map var lettersSim)

    fv (TVar a)   = [a]
    fv (TApp a b) = fv a ++ fv b
    fv (TCon _ _)    = []

    fd a = snd <$> find (\(TV n _, x) -> n == a) ord

    normpred (IsIn n (TVar (TV k _))) = case fd k of
        Just x -> [IsIn n $ TVar x]
        Nothing -> []
    normpred a = error $ "what ? " ++ show a

    normtype (TApp a b) = TApp (normtype a) (normtype b)
    normtype (TCon a k)   = TCon a k
    normtype (TVar (TV a _))   =
      case fd a of
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

generalise :: Env -> [Pred] -> Type -> Scheme
generalise env p t = Forall as $ Qual p t
  where as = Set.toList $ ftv env `Set.difference` ftv t

fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ tvar (letters !! count s)


noConstraints :: Constraints
noConstraints = ([], [])

(+-) :: Constraints -> Constraints -> Constraints
(a, b) +- (a', b') = (a ++ a', b ++ b')

lookupEnv :: Name -> Infer (Qual Type)
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
    Just s -> instantiate s
    Nothing -> throwError $ UnboundVariable $ show x

inferEq :: Expr -> Scheme -> Infer (Type, Constraints)
inferEq e t0 = do
  Qual q t1 <- instantiate t0
  (t2, c) <- infer e
  return (t1, ([], q) +- union (t1, t2))

infer :: Expr -> Infer (Type, Constraints)
infer = \case
  Lit (LInt _) -> return (typeInt, noConstraints)
  Lit (LBool _) -> return (typeBool, noConstraints)

  Var x -> do
      (Qual p t) <- lookupEnv x
      return (t, ([], p))

  Lam x e -> do
    tv <- fresh
    (t, c) <- inEnv (x, Forall [] (Qual [] tv)) (infer e)
    return (tv `mkArr` t, c)

  App e1 e2 -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    return (tv, c1 +- c2 +- union (t1, t2 `mkArr` tv))

  Fix e1 -> do
    (t1, c1) <- infer e1
    tv <- fresh
    return (tv, c1 +- union (tv `mkArr` tv, t1))

type Unifier = (Subst, [Union])

emptyUnifier :: Unifier
emptyUnifier = (nullSubst, [])

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return nullSubst
unifies (TVar v) t = bind v t
unifies t (TVar v) = bind v t
unifies (TApp t1 t2) (TApp t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = do
  throwError $ UnificationFail t1 t2

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return nullSubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  s1 <- unifies t1 t2
  s2 <- unifyMany (apply s1 ts1) (apply s1 ts2)
  return $ s2 `compose` s1
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

solver :: Constraints -> Solve ([Pred], Subst)
solver (unions, ps) = do
  tell $ "solve : \n" ++ pretty unions ++ "\n"
  sub <- unionSolve (nullSubst, unions)
  preds <- withReaderT (\e -> (e, sub)) (classSolve $ ClassSolver [] (apply sub ps))
  return (preds, sub)


unionSolve :: Unifier -> Solve Subst
unionSolve (su, cs) =
  case cs of
    [] -> return su
    Union (t1, t2) : cs1 -> do
      su1 <- unifies t1 t2
      unionSolve (su1 `compose` su, apply su1 cs1)

data ClassSolver = ClassSolver{found :: [Pred], remain :: [Pred]};

classSolve :: ClassSolver -> CSolve [Pred]
classSolve = \case
  ClassSolver founds [] -> return founds
  ClassSolver founds (p:ps) -> do
    preds <- solvePred p
    classSolve $ ClassSolver (founds ++ preds) ps

solvePred :: Pred -> CSolve [Pred]
solvePred (IsIn n t) = do
  (ClassEnv cenv, s) <- ask
  case Map.lookup n cenv of
    Nothing -> throwError $ UnknownClass n
    Just cls -> isIn n (apply s cls) t

isIn :: String -> Class -> Type -> CSolve [Pred]
isIn cname (m, insts) = \case
  TVar t -> return [IsIn cname $ TVar t]
  t -> satisfyInsts (IsIn cname t) insts

satisfyInsts :: Pred -> [Inst] -> CSolve [Pred]
satisfyInsts s [i] = satisfyInst s i
satisfyInsts s (i:is) = satisfyInst s i `catchError` \e -> satisfyInsts s is

satisfyInst :: Pred -> Inst -> CSolve [Pred]
satisfyInst t (Qual ps t') =
  if t == t'
  then classSolve $ ClassSolver [] ps
  else throwError $ NotInClass t

-- tries to unify a and t
bind :: TVar -> Type -> Solve Subst
bind a t | t == TVar a = return nullSubst -- bind a t =
         | occursCheck a t = throwError $ InfiniteType t
         | otherwise = return $ Map.singleton a t

occursCheck :: Substituable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

unifyPred :: Pred -> Pred -> Solve Subst
unifyPred (IsIn i t) (IsIn i' t') | i == i' = unifies t t'
