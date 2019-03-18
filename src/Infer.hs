{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.List (nub, find, length)
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
  | NotInClass String Type
  | UnknownClass String
  | WrongKind String Kind
  | UnificationFail Type Type
  | UndeclaredClass String
  | MultipleDecl String
  | UnknownCommand String
  | SignatureMismatch Subst
  | UnificationMismatch [Type] [Type] deriving (Show, Eq);

instance Pretty TypeError where
  pretty = \case
    UnboundVariable s -> "Variable not in scope : "++ s
    InfiniteType t -> "Cannot create infinite type : "++pretty t
    NotInClass a b -> pretty b ++ " is not in " ++ a
    UnknownCommand s -> "Unknown command : " ++ s
    UnknownClass s -> "Unknown class : " ++ s
    UndeclaredClass s -> "Undeclared class : " ++ s
    WrongKind s k -> s ++ " has kind : " ++ pretty k
    MultipleDecl s -> s ++ " Multiple declarations : " ++ s
    SignatureMismatch s -> "Signature does not match : " ++ pretty s
    UnificationFail t t' -> "Cannot unify : " ++ pretty t ++ " with "++pretty t'
    UnificationMismatch t t' -> "Cannot unify : " ++ pretty t ++ " with "++pretty t'

type ExceptLog a = ExceptT TypeError (Writer String) a;

type Infer a = (ReaderT Env (StateT InferState (Except TypeError)) a)

type Solve a = (ReaderT ClassEnv (ExceptT TypeError (Writer String)) a)

newtype InferState = InferState {count :: Int}

initInfer :: InferState
initInfer = InferState { count = 0 }

runSolve :: ClassEnv -> Constraints -> ExceptLog ([Pred], Subst)
runSolve env cs = runReaderT (solver cs) env

inferDecl :: Envs -> [(String, [String], Expr)] -> ExceptLog Envs
inferDecl env [] = return env
inferDecl (Envs d ev cenv) ((name, _, ex):xs) = do
  e <- inferExpr cenv d ex
  inferDecl (Envs (extend d (name, e)) ev cenv) xs


inferTop :: Envs -> [(String, Expr)] -> ExceptLog Envs
inferTop env [] = return env
inferTop (Envs d env cenv) ((name, ex):xs) = do
  tell $ "infering : " ++ pretty ex ++ "\n"
  e <- case Env.lookup name env of
    Nothing -> inferExpr cenv env ex
    Just sc -> inferExprT cenv env ex sc
  inferTop (Envs d (extend env (name, e)) cenv) xs

inferExpr :: ClassEnv -> Env -> Expr -> ExceptLog Scheme
inferExpr cenv env ex = case runInfer env (infer ex) of
  Left err -> throwError err
  Right (ty, cs) -> do
    (preds, subst) <- runSolve cenv cs
    return $ closeOver (apply subst ty) (apply subst preds)

inferExprT :: ClassEnv -> Env -> Expr -> Scheme -> ExceptLog Scheme
inferExprT cenv env ex tp = case runInfer env (inferEq ex tp) of
  Left err -> throwError err
  Right (found, cs, expected) -> do
    (preds, subst) <- runSolve cenv cs
    s0 <- checkStrict (apply subst found) (apply subst expected) False
    checkSubst s0
    return $ closeOver (apply subst found) (apply subst preds)

runInfer :: Env -> Infer a -> Either TypeError a
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

lookupEnv :: Name -> Infer (Qual Type)
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
    Just s -> instantiate s
    Nothing -> throwError $ UnboundVariable $ show x

inferEq :: Expr -> Scheme -> Infer (Type, Constraints, Type)
inferEq e t0 = do
  Qual q t1 <- instantiate t0
  (t2, c) <- infer e
  return (t2, c, t1)

infer :: Expr -> Infer (Type, Constraints)
infer = \case
  Lit _ -> return (typeInt, noConstraints)

  Case e ex -> do
    (sourceT, c1) <- infer e
    (t:ts, cs) <- unzip <$> mapM infer ex
    let exprEq = mconcat $ curry union t <$> ts
    destT <- fresh
    return (destT, mconcat cs <> union (t, sourceT `mkArr` destT) <> exprEq)

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
    return (tv, c1 <> c2 <> union (t1, t2 `mkArr` tv))

  Fix e1 -> do
    (t1, c1) <- infer e1
    tv <- fresh
    return (tv, c1 <> union (tv `mkArr` tv, t1))

checkSubst :: Subst -> ExceptLog Subst
checkSubst sub = Map.elems >>> nub >>> check $ sub
  where check l = if length l == Map.size sub
                  then return sub
                  else throwError $ SignatureMismatch sub

checkStrict :: Type -> Type -> Bool -> ExceptLog Subst
checkStrict t1 t2 _ | t1 == t2 = return nullSubst
checkStrict (TVar v) t2@(TVar v2) False = return $ Map.singleton v t2
checkStrict (TVar v) t2 True = return $ Map.singleton v t2
checkStrict
  t1@(TApp (TApp (TCon "(->)" _) a) b)
  t2@(TApp (TApp (TCon "(->)" _) a0) b0)
  contra = do
  s1 <- checkStrict a a0 (not contra)
  s2 <- checkStrict (apply s1 b) (apply s1 b0) contra
  return $ s2 `compose` s1
checkStrict t1 t2 _ = throwError $ UnificationFail t1 t2

type Unifier = (Subst, [Union])

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

solver :: Constraints -> Solve ([Pred], Subst)
solver (unions, ps) = do
  tell $ "solve : \n" ++ pretty unions ++ "\n"
  sub <- unionSolve (nullSubst, unions)
  preds <- classSolve $ ClassSolver [] (apply sub ps)
  return (preds, sub)


unionSolve :: Unifier -> Solve Subst
unionSolve (su, cs) =
  case cs of
    [] -> return su
    Union (t1, t2) : cs1 -> do
      tell $ "unify : " ++ pretty t1 ++ " and " ++ pretty t2 ++ "\n"
      su1 <- unifies t1 t2
      tell $ "found : " ++ pretty su1 ++ "\n"
      unionSolve (su1 `compose` su, apply su1 cs1)

data ClassSolver = ClassSolver{found :: [Pred], remain :: [Pred]};

classSolve :: ClassSolver -> Solve [Pred]
classSolve = \case
  ClassSolver founds [] -> return founds
  ClassSolver founds (p:ps) -> do
    preds <- solvePred p
    classSolve $ ClassSolver (founds ++ preds) ps

solvePred :: Pred -> Solve [Pred]
solvePred (IsIn n t) = do
  ClassEnv cenv <- ask
  case Map.lookup n cenv of
    Nothing -> throwError $ UnknownClass n
    Just cls -> isIn n cls t

isIn :: String -> Class -> Type -> Solve [Pred]
isIn cname (m, insts) = \case
  TVar t -> return [IsIn cname $ TVar t]
  t -> satisfyInsts (IsIn cname t) insts

satisfyInsts :: Pred -> [Inst] -> Solve [Pred]
satisfyInsts s [i] = satisfyInst s i
satisfyInsts s (i:is) = satisfyInst s i `catchError` \e -> satisfyInsts s is

satisfyInst :: Pred -> Inst -> Solve [Pred]
satisfyInst (IsIn c t) (Qual ps (IsIn _ t')) = do
  s <- unifies t' t `catchError` const (throwError $ NotInClass c t)
  return $ apply s ps

-- tries to unify a and t
bind :: TVar -> Type -> Solve Subst
bind a t | t == TVar a = return nullSubst -- bind a t =
         | occursCheck a t = throwError $ InfiniteType t
         | otherwise = return $ Map.singleton a t

occursCheck :: Substituable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

unifyPred :: Pred -> Pred -> Solve Subst
unifyPred (IsIn i t) (IsIn i' t') | i == i' = unifies t t'
