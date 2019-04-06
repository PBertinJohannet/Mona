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
import RecursionSchemes

newtype Union = Union (Type, Type) deriving Show;

instance Pretty Union where
  pretty (Union (a, b)) = pretty a ++ " should unify with : " ++ pretty b ++ "\n"

type Constraints = ([Union], [Pred])

instance Pretty Constraints where
  pretty (a, b) = prettyL a ++ " => " ++ prettyL b ++ "\n"

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
    UnificationMismatch t t' -> "Mismatch : Cannot unify : " ++ prettyL t ++ " with "++prettyL t'

type ExceptLog a = ExceptT TypeError (Writer String) a;

type Infer = ReaderT Env (StateT InferState (Except TypeError))
type InferCons = WriterT Constraints Infer

type Solve a = (ReaderT ClassEnv (ExceptT TypeError (Writer String)) a)

newtype InferState = InferState {count :: Int}

initInfer :: InferState
initInfer = InferState { count = 0 }

runSolve :: ClassEnv -> Constraints -> ExceptLog ([Pred], Subst)
runSolve env cs = runReaderT (solver cs) env

checkInstances :: Envs -> [InstCheck] -> ExceptLog Envs
checkInstances env [] = return env
checkInstances (Envs d env cenv tast) ((name, sc, ex):xs) = do
  (_, texp) <- inferExprT cenv env ex sc
  checkInstances (Envs d env cenv (extendAst tast (name, texp))) xs

inferTop :: Envs -> [(String, Expr)] -> ExceptLog Envs
inferTop env [] = return env
inferTop (Envs d env cenv tast) ((name, ex):xs) = do
  --tell $ "infering : " ++ pretty ex ++ "\n"
  (tp, texp) <- case Env.lookup name env of
    Nothing -> inferExpr cenv env ex
    Just sc -> inferExprT cenv env ex sc
  inferTop (Envs d (extend env (name, tp)) cenv (extendAst tast (name, texp))) xs

inferExpr :: ClassEnv -> Env -> Expr -> ExceptLog (Scheme, TExpr)
inferExpr cenv env ex = case runInfer env (runWriterT $ infer ex) of
  Left err -> throwError err
  Right (ty', cs) -> do
    let ty = snd $ ann ty'
    --tell $ "found : "++ pretty ty ++ "\n"
    (preds, subst) <- runSolve cenv cs
    return (closeOver (apply subst ty) (apply subst preds), ty')

inferExprT :: ClassEnv -> Env -> Expr -> Scheme -> ExceptLog (Scheme, TExpr)
inferExprT cenv env ex tp = case runInfer env (runWriterT $ inferEq ex tp) of
  Left err -> throwError err
  Right ((texp, expected), cs) -> do
    (preds, subst) <- runSolve cenv cs
    let found = snd $ ann texp
    --tell $ "found : " ++ pretty (apply subst found) ++ "\n"
    --tell $ "expected : " ++ pretty (apply subst expected) ++ "\n"
    s0 <- checkStrict (apply subst found) (apply subst expected) False
    checkSubst s0
    return (closeOver (apply subst found) (apply subst preds), apply subst texp)

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

lamInEnv :: String -> InferCons Type -> InferCons Type
lamInEnv x e = do
  tv <- fresh
  t <- inEnv (x, Forall [] (Qual [] tv)) e
  return (tv `mkArr` t)

inEnv :: (Name, Scheme) -> InferCons a -> InferCons a
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

instantiate :: Scheme -> InferCons (Qual Type) -- replace by fresh variables
instantiate (Forall as (Qual preds t)) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ Qual (apply s preds) (apply s t)

generalise :: Env -> [Pred] -> Type -> Scheme
generalise env p t = Forall as $ Qual p t
  where as = Set.toList $ ftv env `Set.difference` ftv t

fresh :: InferCons Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ tvar (letters !! count s)


noConstraints :: Constraints
noConstraints = ([], [])

lookupEnv :: Name -> InferCons (Qual Type)
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
    Just s -> instantiate s
    Nothing -> throwError $ UnboundVariable $ show x

inferEq :: Expr -> Scheme -> InferCons (TExpr, Type)
inferEq e t0 = do
  Qual q t1 <- instantiate t0
  t2 <- infer e
  return (t2, t1)

infer :: Expr -> InferCons TExpr
infer = cataCF inferAlg'

getTp :: InferCons TExpr -> InferCons Type
getTp = fmap (\(In ((_, t) :< _)) -> t)

inferAlg' :: (Location, ExprF (InferCons TExpr)) -> InferCons TExpr
inferAlg' = \case
    (l, Lam x e) -> do
        tv <- fresh
        e' <- inEnv (x, Forall [] (Qual [] tv)) e
        t <- getTp $ return e'
        return $ In $ (l, tv `mkArr` t) :< Lam x e'
    (l, k) -> do
        tp <- inferAlg k
        k <- sequence k
        return $ In $ (l, tp) :< k

inferAlg :: ExprF (InferCons TExpr) -> InferCons Type
inferAlg = \case
      Lit _ -> do
        tell noConstraints
        return typeInt

      Case sourceT (t:ts) -> do
        t <- getTp t
        ts <- mapM getTp ts
        sourceT <- getTp sourceT
        let exprEq = mconcat $ curry union t <$> ts
        destT <- fresh
        tell $ union (t, sourceT `mkArr` destT) <> exprEq
        return destT

      Var x -> do
        env <- ask
        (Qual p t) <- lookupEnv x -- `catchError` \e -> lookupEnv $ pretty env
        return t

      App t1 t2 -> do
        t1 <- getTp t1
        t2 <- getTp t2
        tv <- fresh
        tell $ union (t1, t2 `mkArr` tv)
        return tv

      Fix t1 -> do
        t1 <- getTp t1
        tv <- fresh
        tell $ union (tv `mkArr` tv, t1)
        return tv
{- old
infer :: Expr -> InferCons Type
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
-}
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
checkStrict (TApp t1 v1) (TApp t2 v2) c = do
  s1 <- checkStrict t1 t2 c
  s2 <- checkStrict (apply s1 v1) (apply s1 v2) c
  return $ s2 `compose` s1
checkStrict t1 t2 _ = do
  tell $ "try for : " ++ showKind t1 ++ " and " ++ showKind t2 ++ "\n"
  throwError $ UnificationFail t1 t2

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
  --tell $ "solve : \n" ++ prettyL unions ++ "\n"
  sub <- unionSolve (nullSubst, unions)
  preds <- classSolve $ ClassSolver [] (apply sub ps)
  return (preds, sub)


unionSolve :: Unifier -> Solve Subst
unionSolve (su, cs) =
  case cs of
    [] -> return su
    Union (t1, t2) : cs1 -> do
      --tell $ "unify : " ++ pretty t1 ++ " and " ++ pretty t2 ++ "\n"
      su1 <- unifies t1 t2
      --tell $ "found : " ++ pretty su1 ++ "\n"
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
