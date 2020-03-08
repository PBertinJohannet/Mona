{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import Data.Tuple
import Error
import Data.List (intersperse)

newtype Union = Union (Type, Type, Location) deriving Show;
data Reconcile = Reconcile Type Type [(Type, Type)] deriving Show;
type Constraints = ([Union], [(Pred, Location)], [Reconcile])

instance Pretty Union where
  pretty (Union (a, b, _)) = pretty a ++ " should unify with : " ++ pretty b ++ "\n"

instance Pretty Reconcile where
  pretty (Reconcile a b tps) = prettyArr (a, b) ++ "<" ++ mconcat (intersperse "," (prettyArr <$> tps)) ++ ">"
    where prettyArr (a, b) = pretty a ++ " -> " ++ pretty b

instance Pretty (Pred, Location) where
  pretty (a, b) = pretty a

instance Pretty Constraints where
  pretty (a, b, c) = prettyL a ++ " => " ++ prettyL b ++ "[" ++ prettyL c ++ "]\n"

instance Substituable Reconcile where
  apply s (Reconcile a b ts) = Reconcile (apply s a) (apply s b) ((apply s *** apply s) <$> ts)
  ftv _ = Set.empty

union :: Location -> (Type, Type) -> Constraints
union loc (a, b) = ([Union (a, b, loc)], [], [])

predicate :: [Pred] -> Location -> Constraints
predicate p l = ([], (,l) <$> p, [])

reconcilie :: Type -> Type -> [(Type, Type)] -> Constraints
reconcilie a b tps = ([], [], [Reconcile a b tps])

instance Substituable Union where
  apply s (Union (a, b, c)) = Union (apply s a, apply s b, c)
  ftv u = Set.empty

type TypeError = CompilerError TypeErrorV;

data TypeErrorV
  = UnboundVariableInType String
  | UnboundVariable String
  | NotAClassFunction String
  | InfiniteType Type
  | NotInClass String Type
  | UnknownClass String
  | WrongKind String Kind
  | UnificationFail Type Type
  | KindUnificationFail String String
  | UndeclaredClass String
  | MultipleDecl String
  | UnknownCommand String
  | SignatureMismatch Scheme Scheme
  | ConstraintsNotMatching [Pred] deriving (Show, Eq);

instance Pretty TypeErrorV where
  pretty = \case
    UnboundVariableInType s -> "Type variable not in scope : "++ s
    UnboundVariable s -> "Variable not in scope : "++ s
    NotAClassFunction s -> "(NotAClassFunction) Function " ++ s ++ " was not declared in this class "
    InfiniteType t -> "Cannot create infinite type : "++pretty t
    NotInClass a b -> pretty b ++ " is not in " ++ a
    UnknownCommand s -> "Unknown command : " ++ s
    UnknownClass s -> "Unknown class : " ++ s
    UndeclaredClass s -> "Undeclared class : " ++ s
    WrongKind s k -> s ++ " has kind : " ++ pretty k
    MultipleDecl s -> s ++ " Multiple declarations : " ++ s
    SignatureMismatch t t' -> "Cannot match infered signature " ++ pretty t ++ " with declared signature " ++ pretty t'
    UnificationFail t t' -> "Cannot unify : " ++ pretty t ++ " with "++ pretty t'
    KindUnificationFail t t' -> "Cannot unify : " ++ t ++ " with "++ t'
    ConstraintsNotMatching t -> "Infered constraints not found in definition : " ++ prettyL t

type ExceptLog a = ExceptT TypeError (Writer String) a;

type Infer = ReaderT Env (StateT InferState (Except TypeError))
type InferCons = WriterT Constraints Infer

type Solve a = (ReaderT ClassEnv (ExceptT TypeError (Writer String)) a)

newtype InferState = InferState {count :: Int}

initInfer :: InferState
initInfer = InferState { count = 0 }

asSolve :: ExceptLog a -> Solve a
asSolve a = ReaderT $ const a

runSolve :: ClassEnv -> [(Pred, Location)] -> ExceptLog [Pred]
runSolve env cs = runReaderT (solver cs) env

checkInstances :: Envs -> [InstCheck] -> ExceptLog Envs
checkInstances env [] = return env
checkInstances (Envs d env cenv tast) ((loc, name, sc, ex):xs) = do
  tell $ "checking : " ++ pretty sc ++ "\n"
  (_, texp) <- inferExprT cenv env ex sc
  tell $ "adding : " ++ pretty texp ++ "\n"
  checkInstances (Envs d env cenv (extendAst tast (name, texp))) xs

inferTop :: Envs -> [(Location, String, Expr)] -> ExceptLog Envs
inferTop env [] = return env
inferTop (Envs d env cenv tast) ((loc, name, ex):xs) = do
  (tp, texp) <- case Env.lookup name env of
    Nothing -> inferExpr cenv env ex `withErrorLoc` loc
    Just sc -> inferExprT cenv env ex sc `withErrorLoc` loc
  inferTop (Envs d (extend env (name, tp)) cenv (extendAst tast (name, texp))) xs

applyAnn :: (Type -> Type) -> TExpr -> TExpr
applyAnn f = mapAnn applyAnn'
  where 
    applyAnn' :: (Location, Subst, Qual Type) -> (Location, Subst, Qual Type)
    applyAnn' (loc, sub, Qual preds t) = (loc, f <$> sub, Qual (fmap (mapPred f) preds) $ f t)

inferExpr :: ClassEnv -> Env -> Expr -> ExceptLog (Scheme, TExpr)
inferExpr cenv env ex = case runInfer env (runWriterT $ infer ex) of
  Left err -> throwError err
  Right (tyBefore, (unions, preds, dims)) -> do
    let (_, _, Qual _ ty) = ann tyBefore
    tell $ "things : " ++ prettyL dims
    --tell $ "found : "++ pretty ty ++ "\n"
    tell $ "with type : " ++ pretty ty ++ " solve => \n"
    --tell $ "constraints : " ++ pretty preds ++ " go \n"
    subst <- runUnify unions dims
    tell $ "found subst : " ++ pretty subst
    --preds' <- sequence (_vtoT . apply subst <$> preds)
    preds'' <- runSolve cenv (first (apply subst) <$>  preds)
    let ty' = apply subst ty
    let tyBefore' = applyAnn (apply subst) tyBefore
    --tell $ "before : " ++ pretty (apply subst ty') ++ "\n" ++ pretty (apply subst ty) ++ "\n"
    --let (b, s) = closeOver (apply subst ty) (apply subst preds) $ apply subst ty'
    --tell $ "after CO : " ++ pretty b ++ "\n" ++ pretty s ++ "\n"
    return (closeOver ty' preds'' tyBefore')

inferExprT :: ClassEnv -> Env -> Expr -> Scheme -> ExceptLog (Scheme, TExpr)
inferExprT cenv env ex tp = case runInfer env (runWriterT $ inferEq ex tp) of
  Left err -> throwError err
  Right ((texp, expected), (unions, preds, things)) -> do
    tell $ "things : " ++ prettyL things
    tell $ "doing : " ++ prettyL unions ++ "\n"
    subst <- runUnify unions things
    preds <- runSolve cenv $ first (apply subst) <$> preds
    let (_, _, Qual _ found) = ann texp
    let Forall _ (Qual expectedPreds _) = tp
    --tell $ "checking preds : " ++ prettyL preds ++ " vs " ++ prettyL expectedPreds ++ "\n"
    checkPreds preds expectedPreds
    --tell $ "found : " ++ pretty (apply subst found) ++ "\n"
    --tell $ "expected : " ++ pretty (apply subst expected) ++ "\n"
    allReplaces <- checkStrict (apply subst found) (apply subst expected)
    --stest <- checkStrict (apply subst expected) (apply subst found) False
    --tell $ "\nor : " ++ show stest
    let s0 = Map.fromList allReplaces
    --let stest2 = s0 `compose` stest
    --tell $ "\n to give : " ++ pretty stest2
    let s1 = s0 `compose` subst
    --tell $ "sub : " ++ pretty s1 ++ "\n"
    --tell $ "before : " ++ pretty (apply s1 texp) ++ "\n" ++ pretty (apply s1 found) ++ "\n"
    --tell $ "after COC : " ++ pretty b ++ "\n" ++ pretty s ++ "\n"
    let (sch, tex) = closeOver (apply s1 found) (mapPred (apply s1) <$> preds) $ applyAnn (apply s1) texp
    foldM_ (composeStrict sch tp) nullSubst allReplaces
    return (tp, tex)

runInfer :: Env -> Infer a -> Either TypeError a
runInfer env m = runIdentity $ runExceptT $ evalStateT (runReaderT m env) initInfer

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> [Pred] -> TExpr -> (Scheme, TExpr)
closeOver t p s = (normalize . first (generalise p)) (t, s)

generalise :: [Pred] -> Type -> Scheme
generalise p t = Forall [] $ Qual p t

normalize :: (Scheme, TExpr) -> (Scheme, TExpr)
normalize (Forall _ (Qual q body), s) =
  (Forall (snd <$> varSubst) (Qual (q >>= normPred) (normType body)), mapRes normType s)
  where
    varSubst :: [(TVar, TVar)]
    varSubst = zip (nub $ fv body) (map var lettersSim)

    fv :: Type -> [TVar]
    fv = Set.toList . ftv

    lookupVars :: String -> Maybe TVar
    lookupVars a = snd <$> find (\(TV n _, _) -> n == a) varSubst

    normPred :: Pred -> [Pred]
    normPred (IsIn n (TVar (TV k _))) = case lookupVars k of
        Just x -> [IsIn n $ TVar x]
        Nothing -> []
    normPred (IsIn n (TCon _ _)) = []
    normPred a = error $ "what ? " ++ show a

    normType :: Type -> Type
    normType (TApp a b) = TApp (normType a) (normType b)
    normType (TCon a k)   = TCon a k
    normType (TVar (TV a k)) =
      case lookupVars a of
        Just x -> TVar x
        Nothing -> TVar (TV a k)

class InEnv a where
  inEnv :: (Name, a) -> InferCons b -> InferCons b

instance InEnv Type where
  inEnv (x, t) = inEnv (x, Forall [] (Qual [] t))

instance InEnv Scheme where
  inEnv (x, sc) m = do
    let scope e = remove e x `extend` (x, sc)
    local scope m

getSignature :: Scheme -> InferCons (Subst, [Pred], Type)
getSignature (Forall as (Qual preds t)) = do
    as' <- mapM (const freshType) as
    let s = Map.fromList $ zip as as'
    return (s, preds, t)

instantiate :: Scheme -> InferCons (Subst, Qual Type) -- replace by fresh variables
instantiate (Forall as (Qual preds t)) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return (s, Qual (apply s preds) (apply s t))

freshName :: InferCons String
freshName = do
  s <- get
  put s{count = count s + 1}
  return (letters !! count s)

fresh :: InferCons Type
fresh = freshType

freshType :: InferCons Type
freshType = tvar <$> freshName

freshQual :: InferCons (Qual Type)
freshQual = Qual [] <$> fresh

noConstraints :: Constraints
noConstraints = ([], [], [])

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
getTp (In ((_, _, Qual _ t) :< _)) = t

getVTp :: TExpr -> Type
getVTp (In ((_, _, Qual _ t) :< _)) = t

noSub :: Type -> InferCons (Qual Type, Subst)
noSub = return . (, nullSubst) . Qual []

inferAlgM :: (Location, ExprF (InferCons TExpr)) -> InferCons TExpr
inferAlgM = \case
    (l, Case sourceT xs) -> do      -- patType = sourceType -> ret

      (trets, pats) <- unzip <$> traverse (inferPat l) xs
      ret <- fresh
      sourceType <- getTp <$> sourceT
      -- patType = sourceType -> ret
      tell $ reconcilie sourceType ret trets

      pats' <- return (fmap return <$> pats)
      retExpr <- sequence $ Case sourceT pats'
      return (In $ (l, nullSubst, Qual [] ret) :< retExpr)

    (l, Lam pat) -> do
      ((tin, tout), pat') <- inferPat l pat
      return $ In ((l, nullSubst, Qual [] (tin `mkArr` tout)) :< Lam pat')

    (l, k) -> do
        k <- sequence k
        let tpk = getVTp <$> k
        (tp, sub) <- inferAlg l tpk
        return (In $ (l, sub, tp) :< k)
  where
    inferAlg :: Location -> ExprF Type -> InferCons (Qual Type, Subst)
    inferAlg l = \case
          Lit _ -> do
            tell noConstraints
            noSub typeInt

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
{-
inPatEnv :: Type -> PatternT (InferCons TExpr) -> PatternT (InferCons TExpr)
inPatEnv tv (PatternT x@(Pattern p ps) e) = case ps of
  [] -> let e' = inEnv (p, Forall [] (Qual [] tv)) e in PatternT x e'
-}

typeOfRet :: String -> InferCons Type
typeOfRet = fmap getReturn . typeOf

typeOf :: String -> InferCons Type
typeOf x = do
    (sub, Qual p t) <- lookupEnv x
    return t

decomposePattern :: Location -> Pattern -> InferCons (Type, [(String, Type)])
decomposePattern loc (Pattern name vars) = do
    subPats <- traverse (decomposePattern loc) vars
    let (subTypes, varsTypes) = unzip subPats
    tname <- typeOf name
    tret <- typeOfRet name
    tell (union loc (foldr mkArr tret subTypes, tname))
    return (tret, mconcat varsTypes)
decomposePattern loc (Raw name) = do
    tret <- freshType
    return (tret, return (name, tret))

inferPat :: Location -> PatternT (InferCons TExpr) -> InferCons ((Type, Type), (PatternT TExpr))
inferPat loc (PatternT pat exp) = do
  -- tp = in type, eg: Bool
    (tp, vars) <- decomposePattern loc pat
    tret <- foldl (flip inEnv) exp vars
    return ((tp, getTp tret), PatternT pat tret)

-- see the doc for the error message, basicaly it verifies that the same variable is not binded to multiple types
-- then it adds it to the given substition
-- the two first args are only to report errors
composeStrict :: Scheme -> Scheme -> Subst -> (TVar, Type) -> ExceptLog Subst
composeStrict sc sc' sub (a, b) = check $ Map.lookup a sub
  where
    check :: Maybe Type -> ExceptLog Subst
    check (Just b') | b' == b = return sub
    check Nothing = return $ Map.insert a b sub
    check _  = throwErrorV $ SignatureMismatch sc sc'

checkStrict :: Type -> Type -> ExceptLog [(TVar, Type)]
checkStrict t1 t2 | t1 == t2 = return []
checkStrict (TVar v) t2 = return [(v, t2)]
checkStrict (TApp t1 v1) (TApp t2 v2) = do
  s1 <- checkStrict t1 t2
  s2 <- checkStrict v1 v2
  return $ s2 ++ s1
checkStrict t1 t2 = throwErrorV $ UnificationFail t1 t2

checkPreds :: [Pred] -> [Pred] -> ExceptLog ()
checkPreds found expected =
  let (f, e) = (Set.fromList found, Set.fromList expected)
      remaining = (f `Set.difference` e) in
  if Set.empty == remaining
    then return ()
    else throwErrorV $ ConstraintsNotMatching $ Set.toList remaining


type Unifier = (Subst, [Union])

emptyUnifier :: Unifier
emptyUnifier = (nullSubst, [])

unifies :: Type -> Type -> ExceptLog Subst
unifies t1 t2 | t1 == t2 = return nullSubst
unifies (TVar v) t = bind v t
unifies t (TVar v) = bind v t
unifies (TApp t1 t2) (TApp t3 t4) = unifyMany (t1, t3) (t2, t4)
unifies t1 t2 = throwErrorV $ UnificationFail t1 t2

unifyMany :: (Type, Type) -> (Type, Type) -> ExceptLog Subst
unifyMany (t1, t1') (t2, t2') = do
  s1 <- unifies t1 t1'
  s2 <- unifies (apply s1 t2) (apply s1 t2')
  return $ s2 `compose` s1

runUnify :: [Union] -> [Reconcile] -> ExceptLog Subst
runUnify [] [] = return nullSubst
runUnify [] (r:rs) = do
  tell " ========= start solving =========\n\n "
  tell $ "reconciling : " ++ pretty r
  sub <- runReconcile r
  tell $ "sus is then : " ++ pretty sub
  sub' <- runUnify [] (apply sub rs)
  tell $ "sub' is then : " ++ pretty sub'
  return (sub' `compose` sub)
runUnify unions recs = do
  tell " ========= finish solving =========\n\n "
  sub <- unionSolve (nullSubst, unions)
  sub' <- runUnify [] (apply sub recs)
  return (sub' `compose` sub)

solver :: [(Pred, Location)] -> Solve [Pred]
solver ps = do
  preds <- classSolve $ ClassSolver [] ps
  tell $ "found preds : " ++ prettyL preds ++ "\n"
  return preds

sequenceFirst :: Functor f => (f a, b) -> f (a, b)
sequenceFirst (a, b) = (,b) <$> a

unionSolve :: Unifier -> ExceptLog Subst
unionSolve (su, cs) =
  case cs of
    [] -> return su
    Union (t1, t2, loc) : cs1 -> do
      tell $ "unify : " ++ pretty t1 ++ " and " ++ pretty t2 ++ "\n"
      su1 <- unifies t1 t2 `withErrorLoc` loc
      tell $ "found : " ++ pretty su1 ++ "\n"
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
  s <- asSolve (unifies t' t `catchError` const (throwErrorV $ NotInClass c t))
  return $ apply s ps

bind :: TVar -> Type -> ExceptLog Subst
bind a t | t == TVar a = return nullSubst
         | occursCheck a t = throwErrorV $ InfiniteType t 
         | otherwise = return $ Map.singleton a t

occursCheck :: TVar -> Type -> Bool
occursCheck a t = a `Set.member` ftv t

unifyPred :: Pred -> Pred -> ExceptLog Subst
unifyPred (IsIn i t) (IsIn i' t') | i == i' = unifies t t'

runReconcile :: Reconcile -> ExceptLog Subst
runReconcile (Reconcile from to ts) = do
  --tell $ "merge : " ++ prettyL ts ++ "\n"
  final <- mergeTypes ts
  tell $ "found : " ++ pretty final ++ "\n"
  subst <- unifies final (from `mkArr` to)
  return subst

mergeTypes :: [(Type, Type)] -> ExceptLog Type
mergeTypes [] = return $ TVar (TV "''empty" (KVar "''empty"))
mergeTypes (r:rs) = uncurry mkArr <$> foldM mergeType r rs

mergeType :: (Type, Type) -> (Type, Type) -> ExceptLog (Type, Type)
mergeType a b | a == b = return a
mergeType (a, b) (a2, b2) = do ok correct this pls, ty :)
  tell $ "mergin : " ++ pretty a ++ " and " ++ pretty a2
  let sec = replaceType a2 a b2
  sub' <- unifies b sec
  return $ apply sub' tp
mergeType left@(TApp (TApp (TCon "(->)" _) a) b) 
          right@(TApp (TApp (TCon "(->)" _) a2) b2) = do
  let myVar = TVar $ TV "'merge" Star
  let left' = replaceType a myVar left
  let right' = replaceType a2 myVar right
  sub <- unifies left' right'
  return $ apply sub left'


mergeLeft :: Type -> Type -> ExceptLog Subst
mergeLeft = unifies