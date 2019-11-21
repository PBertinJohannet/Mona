{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
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
import Data.List (nub, find, length, intercalate)
import Pretty
import RecursionSchemes

newtype Union = Union (Type, Type, Location) deriving Show;

instance Pretty Union where
  pretty (Union (a, b, _)) = pretty a ++ " should unify with : " ++ pretty b ++ "\n"

type Constraints = ([Union], [(Pred, Location)])

instance Pretty (Pred, Location) where
  pretty (a, b) = pretty a

instance Pretty Constraints where
  pretty (a, b) = prettyL a ++ " => " ++ prettyL b ++ "\n"

union :: Location -> (Type, Type) -> Constraints
union loc (a, b) = ([Union (a, b, loc)], [])

predicate :: [Pred] -> Location -> Constraints
predicate p l = ([], (,l) <$> p)

instance Substituable Union where
  apply s (Union (a, b, c)) = Union (apply s a, apply s b, c)
  ftv (Union (a, b, _)) = ftv a `Set.union` ftv b

instance Substituable (Pred, Location) where
  apply s (a, b) = (apply s a, b)
  ftv (a, _) = ftv a

data TypeError = TypeError TypeErrorV [Location];

instance Pretty TypeError where
  pretty (TypeError v []) = pretty v ++ " at <no location info>"
  pretty (TypeError v loc) = pretty v ++ " at " ++ intercalate " at " (pretty <$> loc)

throwErrorV :: MonadError TypeError m => TypeErrorV -> m a
throwErrorV variant = throwError (TypeError variant [])

withErrorLoc :: MonadError TypeError m => m a -> Location -> m a
withErrorLoc a loc = a `catchError` withLoc
  where
    withLoc (TypeError variant a) = throwError $ TypeError variant (loc:a)

data TypeErrorV
  = UnboundVariableInType String
  | UnboundVariable String
  | InfiniteType Type
  | NotInClass String Type
  | UnknownClass String
  | WrongKind String Kind
  | UnificationFail Type Type
  | KindUnificationFail String String
  | UndeclaredClass String
  | MultipleDecl String
  | UnknownCommand String
  | SignatureMismatch Subst
  | UnificationMismatch [Type] [Type] deriving (Show, Eq);

instance Pretty TypeErrorV where
  pretty = \case
    UnboundVariableInType s -> "Type variable not in scope : "++ s
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
    KindUnificationFail t t' -> "Cannot unify : " ++ t ++ " with "++ t'
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
checkInstances (Envs d env cenv tast) ((loc, name, sc, ex):xs) = do
  (_, texp) <- inferExprT cenv env ex sc
  checkInstances (Envs d env cenv (extendAst tast (name, texp))) xs

inferTop :: Envs -> [(Location, String, Expr)] -> ExceptLog Envs
inferTop env [] = return env
inferTop (Envs d env cenv tast) ((loc, name, ex):xs) = do
  (tp, texp) <- case Env.lookup name env of
    Nothing -> inferExpr cenv env ex `withErrorLoc` loc
    Just sc -> inferExprT cenv env ex sc `withErrorLoc` loc
  inferTop (Envs d (extend env (name, tp)) cenv (extendAst tast (name, texp))) xs

inferExpr :: ClassEnv -> Env -> Expr -> ExceptLog (Scheme, TExpr)
inferExpr cenv env ex = case runInfer env (runWriterT $ infer ex) of
  Left err -> throwError err
  Right (ty', cs) -> do
    let (_, _, Qual _ ty) = ann ty'
    --tell $ "found : "++ pretty ty ++ "\n"
    tell $ "with type : " ++ pretty ty' ++ " solve => \n"
    tell $ "constraints : " ++ pretty cs ++ " go \n"
    (preds, subst) <- runSolve cenv cs
    tell $ "before : " ++ pretty (apply subst ty') ++ "\n" ++ pretty (apply subst ty) ++ "\n"
    let (b, s) = closeOver (apply subst ty) (apply subst preds) $ apply subst ty'
    tell $ "after CO : " ++ pretty b ++ "\n" ++ pretty s ++ "\n"
    return (closeOver (apply subst ty) (apply subst preds) $ apply subst ty')

inferExprT :: ClassEnv -> Env -> Expr -> Scheme -> ExceptLog (Scheme, TExpr)
inferExprT cenv env ex tp = case runInfer env (runWriterT $ inferEq ex tp) of
  Left err -> throwError err
  Right ((texp, expected), cs) -> do
    --tell $ "doing : " ++ pretty ex ++ "\n"
    (preds, subst) <- runSolve cenv cs
    let (_, _, Qual _ found) = ann texp
    --tell $ "found : " ++ pretty (apply subst found) ++ "\n"
    --tell $ "expected : " ++ pretty (apply subst expected) ++ "\n"
    s0 <- checkStrict (apply subst found) (apply subst expected) False
    checkSubst s0
    let s1 = s0 `compose` subst
    --tell $ "sub : " ++ pretty s1 ++ "\n"
    --tell $ "before : " ++ pretty (apply s1 texp) ++ "\n" ++ pretty (apply s1 found) ++ "\n"
    let (b, s) = closeOver (apply s1 found) (apply s1 preds) $ apply s1 texp
    --tell $ "after COC : " ++ pretty b ++ "\n" ++ pretty s ++ "\n"
    return (closeOver (apply s1 found) (apply s1 preds) $ apply s1 texp)

runInfer :: Env -> Infer a -> Either TypeError a
runInfer env m = runIdentity $ runExceptT $ evalStateT (runReaderT m env) initInfer

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> [Pred] -> TExpr -> (Scheme, TExpr)
closeOver t p s = (normalize . generalise Env.empty p) (t, s)

normalize :: (Scheme, TExpr) -> (Scheme, TExpr)
normalize (Forall _ (Qual q body), s) =
  (Forall (map snd ord) (Qual (q >>= normpred) (normtype body)),
  mapRes normtype s)
  where
    ord = zip (nub $ fv body) (map var lettersSim)

    fv (TVar a)   = [a]
    fv (TApp a b) = fv a ++ fv b
    fv (TCon _ _)    = []

    fd a = snd <$> find (\(TV n _, x) -> n == a) ord

    normpred (IsIn n (TVar (TV k _))) = case fd k of
        Just x -> [IsIn n $ TVar x]
        Nothing -> []
    normpred (IsIn n (TCon _ _)) = []
    normpred a = error $ "what ? " ++ show a

    normtype (TApp a b) = TApp (normtype a) (normtype b)
    normtype (TCon a k)   = TCon a k
    normtype (TVar (TV a k)) =
      case fd a of
        Just x -> TVar x
        Nothing -> TVar (TV a k)

inEnv :: (Name, Scheme) -> InferCons a -> InferCons a
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

instantiate :: Scheme -> InferCons (Subst, Qual Type) -- replace by fresh variables
instantiate (Forall as (Qual preds t)) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return (s, Qual (apply s preds) (apply s t))

generalise :: Env -> [Pred] -> (Type, TExpr) -> (Scheme, TExpr)
generalise env p (t, s) = (Forall as $ Qual p t, s)
  where as = Set.toList $ ftv env `Set.difference` ftv t

fresh :: InferCons Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ tvar (letters !! count s)

noConstraints :: Constraints
noConstraints = ([], [])

lookupEnv :: Name -> InferCons (Subst, Qual Type)
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
    Just s -> instantiate s
    Nothing -> throwErrorV $ UnboundVariable $ show x

inferEq :: Expr -> Scheme -> InferCons (TExpr, Type)
inferEq e t0 = do
  (_, Qual q t1) <- instantiate t0
  t2 <- infer e
  return (t2, t1)

infer :: Expr -> InferCons TExpr
infer = cataCF inferAlgM

getTp :: TExpr -> Type
getTp (In ((_, _, Qual q t) :< _)) = t

noSub :: Type -> InferCons (Qual Type, Subst)
noSub = return . (, nullSubst) . Qual []

inferAlgM :: (Location, ExprF (InferCons TExpr)) -> InferCons TExpr
inferAlgM = \case
    (l, Lam x e) -> do
        tv <- fresh
        e' <- inEnv (x, Forall [] (Qual [] tv)) e
        let t = getTp e'
        return $ In $ (l, nullSubst, Qual [] $ tv `mkArr` t) :< Lam x e'
    (l, k) -> do
        k <- sequence k
        let tpk = getTp <$> k
        (tp, sub) <- inferAlg l tpk
        return (In $ (l, sub, tp) :< k)

inferAlg :: Location -> ExprF Type -> InferCons (Qual Type, Subst)
inferAlg l = \case
      Lit _ -> do
        tell noConstraints
        noSub typeInt

      Case sourceT (t:ts) -> do
        let exprEq = mconcat $ curry (union l) t <$> ts
        destT <- fresh
        tell $ union l (t, sourceT `mkArr` destT) <> exprEq
        noSub destT

      Var x -> do
        env <- ask
        (sub, Qual p t) <- lookupEnv x -- `catchError` \e -> lookupEnv $ pretty env
        tell $ predicate p l
        return (Qual p t, sub)

      App t1 t2 -> do
        tv <- fresh
        tell $ union l (t1, t2 `mkArr` tv)
        noSub tv

      Fix t1 -> do
        tv <- fresh
        tell $ union l (tv `mkArr` tv, t1)
        noSub tv

checkSubst :: Subst -> ExceptLog Subst
checkSubst sub = Map.elems >>> nub >>> check $ sub
  where check l = if length l == Map.size sub
                  then return sub
                  else throwErrorV $ SignatureMismatch sub

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
checkStrict t1 t2 _ = throwErrorV $ UnificationFail t1 t2

type Unifier = (Subst, [Union])

emptyUnifier :: Unifier
emptyUnifier = (nullSubst, [])

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return nullSubst
unifies (TVar v) t = bind v t
unifies t (TVar v) = bind v t
unifies (TApp t1 t2) (TApp t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwErrorV $ UnificationFail t1 t2

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return nullSubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  s1 <- unifies t1 t2
  s2 <- unifyMany (apply s1 ts1) (apply s1 ts2)
  return $ s2 `compose` s1
unifyMany t1 t2 = throwErrorV $ UnificationMismatch t1 t2

solver :: Constraints -> Solve ([Pred], Subst)
solver (unions, ps) = do
  tell " ========= start solving =========\n\n "
  tell $ "preds : " ++ show ps ++ "\n"
  sub <- unionSolve (nullSubst, unions)
  tell $ "found sub : \n" ++ pretty sub ++ "\n"
  tell $ "solve cls : \n" ++ prettyL (apply sub ps) ++ "\n"
  preds <- classSolve $ ClassSolver [] (apply sub ps)
  tell $ "found preds : " ++ prettyL preds ++ "\n"
  return (preds, sub)

unionSolve :: Unifier -> Solve Subst
unionSolve (su, cs) =
  case cs of
    [] -> return su
    Union (t1, t2, loc) : cs1 -> do
      --tell $ "unify : " ++ pretty t1 ++ " and " ++ pretty t2 ++ "\n"
      su1 <- unifies t1 t2 `withErrorLoc` loc
      --tell $ "found : " ++ pretty su1 ++ "\n"
      unionSolve (su1 `compose` su, apply su1 cs1)

data ClassSolver = ClassSolver{found :: [Pred], remain :: [(Pred, Location)]};

classSolve :: ClassSolver -> Solve [Pred]
classSolve = \case
  ClassSolver founds [] -> return founds
  ClassSolver founds ((p, loc):ps) -> do
    preds <- solvePred p `withErrorLoc` loc
    classSolve (ClassSolver (founds ++ preds) ps) `withErrorLoc` loc

solvePred :: Pred -> Solve [Pred]
solvePred (IsIn n t) = do
  ClassEnv cenv <- ask
  case Map.lookup n cenv of
    Nothing -> throwErrorV $ UnknownClass n
    Just cls -> isIn n cls t

isIn :: String -> Class -> Type -> Solve [Pred]
isIn cname (m, insts) e = do
  tell $ "trying to satisfy : " ++ cname ++ " " ++ show e ++ " " ++ show insts ++ "\n"
  case e of
    TVar t -> return [IsIn cname $ TVar t]
    t -> satisfyInsts (IsIn cname t) insts

satisfyInsts :: Pred -> [Inst] -> Solve [Pred]
satisfyInsts (IsIn c t) [] = throwErrorV $ NotInClass c t
satisfyInsts s [i] = satisfyInst s i
satisfyInsts s (i:is) = satisfyInst s i `catchError` \e -> satisfyInsts s is

satisfyInst :: Pred -> Inst -> Solve [Pred]
satisfyInst (IsIn c t) q@(Qual ps (IsIn _ t')) = do
  s <- unifies t' t `catchError` const (throwErrorV $ NotInClass c t)
  return $ apply s ps

-- tries to unify a and t
bind :: TVar -> Type -> Solve Subst
bind a t | t == TVar a = return nullSubst -- bind a t =
         | occursCheck a t = throwErrorV $ InfiniteType t
         | otherwise = return $ Map.singleton a t

occursCheck :: Substituable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

unifyPred :: Pred -> Pred -> Solve Subst
unifyPred (IsIn i t) (IsIn i' t') | i == i' = unifies t t'
