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

newtype Union = Union (Variational, Variational, Location) deriving Show;

instance Pretty Union where
  pretty (Union (a, b, _)) = pretty a ++ " should unify with : " ++ pretty b ++ "\n"

type Constraints a = ([Union], [(Pred a, Location)])
type VConstraints = Constraints Variational
type TConstraints = Constraints Type

instance Pretty a => Pretty (Pred a, Location) where
  pretty (a, b) = pretty a

instance Pretty a => Pretty (Constraints a) where
  pretty (a, b) = prettyL a ++ " => " ++ prettyL b ++ "\n"

union :: Location -> (Variational, Variational) -> Constraints Variational
union loc (a, b) = ([Union (a, b, loc)], [])

predicate :: [Pred Variational] -> Location -> Constraints Variational
predicate p l = ([], (,l) <$> p)

instance Substituable Union Variational where
  apply s (Union (a, b, c)) = Union (apply s a, apply s b, c)

instance Parametrized Union where
  ftv (Union (a, b, _)) = ftv a `Set.union` ftv b

instance Substituable (Pred Variational, Location) Variational where
  apply s (a, b) = (apply s a, b)

instance Parametrized (Pred Variational, Location) where
  ftv (a, _) = ftv a

type TypeError = CompilerError TypeErrorV;

data TypeErrorV
  = UnboundVariableInType String
  | UnboundVariable String
  | NotAClassFunction String
  | InfiniteType Type
  | NotInClass String Type
  | UnknownClass String
  | WrongKind String Kind
  | UnificationFail String String
  | KindUnificationFail String String
  | UndeclaredClass String
  | MultipleDecl String
  | UnknownCommand String
  | SignatureMismatch Scheme Scheme
  | ConstraintsNotMatching [Pred Type] deriving (Show, Eq);

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
    UnificationFail t t' -> "Cannot unify : " ++ t ++ " with "++ t'
    KindUnificationFail t t' -> "Cannot unify : " ++ t ++ " with "++ t'
    ConstraintsNotMatching t -> "Infered constraints not found in definition : " ++ prettyL t

type ExceptLog a = ExceptT TypeError (Writer String) a;

type Infer = ReaderT Env (StateT InferState (Except TypeError))
type InferCons = WriterT (Constraints Variational) Infer

type Solve a = (ReaderT ClassEnv (ExceptT TypeError (Writer String)) a)

newtype InferState = InferState {count :: Int}

initInfer :: InferState
initInfer = InferState { count = 0 }

runSolve :: ClassEnv -> [(Pred Type, Location)] -> ExceptLog [Pred Type]
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

applyAnn :: (Variational -> Type) -> VExpr -> TExpr
applyAnn f = mapAnn applyAnn'
  where 
    applyAnn' :: (Location, VSubst, Qual Variational Variational) -> (Location, TSubst, Qual Type Type)
    applyAnn' (loc, sub, Qual preds t) = (loc, f <$> sub, Qual (fmap (fmap f) preds) $ f t)

inferExpr :: ClassEnv -> Env -> Expr -> ExceptLog (Scheme, TExpr)
inferExpr cenv env ex = case runInfer env (runWriterT $ infer ex) of
  Left err -> throwError err
  Right (tyBefore, (unions, preds)) -> do
    let (_, _, Qual _ ty) = ann tyBefore
    --tell $ "found : "++ pretty ty ++ "\n"
    tell $ "with type : " ++ pretty ty ++ " solve => \n"
    tell $ "constraints : " ++ pretty preds ++ " go \n"
    subst <- runUnify unions
    preds' <- mapM subst preds
    preds'' <- runSolve cenv preds'
    ty' <- (subst ty)
    tyBefore' <- applyAnn subst tyBefore
    --tell $ "before : " ++ pretty (apply subst ty') ++ "\n" ++ pretty (apply subst ty) ++ "\n"
    --let (b, s) = closeOver (apply subst ty) (apply subst preds) $ apply subst ty'
    --tell $ "after CO : " ++ pretty b ++ "\n" ++ pretty s ++ "\n"
    return (closeOver ty' preds'' tyBefore')

inferExprT :: ClassEnv -> Env -> Expr -> Scheme -> ExceptLog (Scheme, TExpr)
inferExprT cenv env ex tp = case runInfer env (runWriterT $ inferEq ex tp) of
  Left err -> throwError err
  Right ((texp, expected), cs) -> do
    --tell $ "doing : " ++ pretty ex ++ "\n"
    (preds, subst) <- runSolve cenv cs
    let (_, _, Qual _ found) = ann texp
    let Forall _ (Qual expectedPreds _) = tp
    --tell $ "checking preds : " ++ prettyL preds ++ " vs " ++ prettyL expectedPreds ++ "\n"
    checkPreds preds expectedPreds
    --tell $ "found : " ++ pretty (apply subst found) ++ "\n"
    --tell $ "expected : " ++ pretty (apply subst expected) ++ "\n"
    allReplaces <- checkStrict (subst found) (subst $ asVariational expected)
    --stest <- checkStrict (apply subst expected) (apply subst found) False
    --tell $ "\nor : " ++ show stest
    let s0 = Map.fromList allReplaces
    --let stest2 = s0 `compose` stest
    --tell $ "\n to give : " ++ pretty stest2
    let s1 = apply s0 . subst
    --tell $ "sub : " ++ pretty s1 ++ "\n"
    --tell $ "before : " ++ pretty (apply s1 texp) ++ "\n" ++ pretty (apply s1 found) ++ "\n"
    --tell $ "after COC : " ++ pretty b ++ "\n" ++ pretty s ++ "\n"
    let (sch, tex) = closeOver (s1 found) (fmap (s1 . asVariational) <$> preds) $ applyAnn s1 texp
    foldM_ (composeStrict sch tp) nullSubst allReplaces
    return (tp, tex)

runInfer :: Env -> Infer a -> Either TypeError a
runInfer env m = runIdentity $ runExceptT $ evalStateT (runReaderT m env) initInfer

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> [Pred Type] -> TExpr -> (Scheme, TExpr)
closeOver t p s = (normalize . first (generalise p)) (t, s)

generalise :: [Pred Type] -> Type -> Scheme
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

    normPred :: Pred Type -> [Pred Type]
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

getSignature :: Scheme -> InferCons (Subst Type, [Pred Type], Type)
getSignature (Forall as (Qual preds t)) = do
    as' <- mapM (const freshType) as
    let s = Map.fromList $ zip as as'
    return (s, preds, t)

class Instanciable a where
  instantiate :: Scheme -> InferCons (Subst a, Qual a a)

instance Instanciable Variational where
  instantiate sc = do
    (s, preds, t) <- getSignature sc
    let vs = asVariational <$> s
    return (vs, Qual (apply s (fmap asVariational <$> preds)) (asVariational $ apply s t))

instance Instanciable Type where
  instantiate sc = do
    (s, preds, t) <- getSignature sc
    return (s, Qual (apply s preds) (apply s t))


freshName :: InferCons String
freshName = do
  s <- get
  put s{count = count s + 1}
  return (letters !! count s)

freshDim :: NonEmpty Variational -> InferCons Variational
freshDim l = do 
  s <- freshName
  return (TPlus $ Dim s l)

fresh :: InferCons Variational
fresh = asVariational <$> freshType

freshType :: InferCons Type
freshType = tvar <$> freshName

freshVType :: InferCons Variational
freshVType = (asVariational . tvar) <$> freshName

freshQual :: InferCons (Qual Variational Variational)
freshQual = Qual [] <$> fresh

noConstraints :: Constraints a
noConstraints = ([], [])

lookupEnv :: Name -> InferCons (VSubst, Qual Variational Variational)
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
    Just s -> instantiate s
    Nothing -> throwErrorV $ UnboundVariable $ show x

inferEq :: Expr -> Scheme -> InferCons (VExpr, Type)
inferEq e t0 = do
  (_, Qual q t1) <- instantiate t0
  t2 <- infer e
  return (t2, t1)

infer :: Expr -> InferCons VExpr
infer = cataCF inferAlgM

getTp :: TExpr -> Type
getTp (In ((_, _, Qual _ t) :< _)) = t

getVTp :: VExpr -> Variational
getVTp (In ((_, _, Qual _ t) :< _)) = t

noSub :: Variational -> InferCons (Qual Variational Variational, VSubst)
noSub = return . (, nullSubst) . Qual []

inferAlgM :: (Location, ExprF (InferCons VExpr)) -> InferCons VExpr
inferAlgM = \case
    (l, Case sourceT []) -> do
      destT <- fresh
      retExpr <- sequence $ Case sourceT []
      return (In $ (l, nullSubst, Qual [] destT) :< retExpr)

    (l, Case sourceT (x:xs)) -> do
      (trets, pats) <- unzipNonEmpty <$> traverse (inferPat l) (x :+: xs)
      infer2 <- freshDim trets
      pats' <- return $ fmap (fmap return) $ asList pats
      retExpr <- sequence $ Case sourceT pats'
      return (In $ (l, nullSubst, Qual [] infer2) :< retExpr)

    (l, Lam pat) -> do
      (tret, pat') <- inferPat l pat
      return $ In ((l, nullSubst, Qual [] tret) :< Lam pat')

    (l, k) -> do
        k <- sequence k
        let tpk = getVTp <$> k
        (tp, sub) <- inferAlg l tpk
        return (In $ (l, sub, tp) :< k)
  where
    inferAlg :: Location -> ExprF Variational -> InferCons (Qual Variational Variational, VSubst)
    inferAlg l = \case
          Lit _ -> do
            tell noConstraints
            noSub $ asVariational typeInt

          Var x -> do
            env <- ask
            (sub, Qual p t) <- lookupEnv x -- `catchError` \e -> lookupEnv $ pretty env
            tell $ predicate p l
            return (Qual p t, sub)

          App t1 t2 -> do
            tv <- fresh
            tell $ union l (t1, t2 `mkVArr` tv)
            noSub tv

          Fix t1 -> do
            tv <- fresh
            tell $ union l (tv `mkVArr` tv, t1)
            noSub tv
{-
inPatEnv :: Type -> PatternT (InferCons VExpr) -> PatternT (InferCons VExpr)
inPatEnv tv (PatternT x@(Pattern p ps) e) = case ps of
  [] -> let e' = inEnv (p, Forall [] (Qual [] tv)) e in PatternT x e'
-}

typeOfRet :: String -> InferCons Variational
typeOfRet = fmap getReturn . typeOf

typeOf :: String -> InferCons Variational
typeOf x = do
    (sub, Qual p t) <- lookupEnv x
    return t

decomposePattern :: Location -> Pattern -> InferCons (Variational, [(String, Type)])
decomposePattern loc (Pattern name vars) = do
    subPats <- traverse (decomposePattern loc) vars
    let (subTypes, varsTypes) = unzip subPats
    tname <- typeOf name
    tret <- typeOfRet name
    tell (union loc (foldr mkVArr tret subTypes, tname))
    return (tret, mconcat varsTypes)
decomposePattern loc (Raw name) = do
    tret <- freshType
    return (asVariational tret, return (name, tret))

inferPat :: Location -> PatternT (InferCons VExpr) -> InferCons (Variational, (PatternT VExpr))
inferPat loc (PatternT pat exp) = do
    (tp, vars) <- decomposePattern loc pat
    tret <- foldl (flip inEnv) exp vars
    return (tp, PatternT pat tret)

-- see the doc for the error message, basicaly it verifies that the same variable is not binded to multiple types
-- then it adds it to the given substition
-- the two first args are only to report errors
composeStrict :: Scheme -> Scheme -> TSubst -> (TVar, Type) -> ExceptLog TSubst
composeStrict sc sc' sub (a, b) = check $ Map.lookup a sub
  where
    check :: Maybe Type -> ExceptLog TSubst
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
checkStrict t1 t2 = throwErrorV $ UnificationFail (pretty t1) (pretty t2)

checkPreds :: [Pred Type] -> [Pred Type] -> ExceptLog ()
checkPreds found expected =
  let (f, e) = (Set.fromList found, Set.fromList expected)
      remaining = (f `Set.difference` e) in
  if Set.empty == remaining
    then return ()
    else throwErrorV $ ConstraintsNotMatching $ Set.toList remaining


type Unifier = (VSubst, [Union])

emptyUnifier :: Unifier
emptyUnifier = (nullSubst, [])

{-
reconcilie :: Variational -> Variational -> Solve Variational
reconcilie a b =
  let (aBase, aArgs) = unapply a in
  let (bBase, bArgs) = unapply b in
  if length bArgs != length aArgs
    then throwErrorV $ ReconciliationError a b
-}

-- TODO : put unifies in a class => Type => Variational

class (Parametrized a, Eq a, Substitute (TypeF a)) => PartialUnify a where
  unifiesPartial :: a -> TypeF a -> Solve (Subst (TypeF a))

class Unifiable a where
  unifies :: a -> a -> Solve (Subst a)

instance (Pretty a, Substituable (TypeF a) (TypeF a), PartialUnify a) => Unifiable (TypeF a) where
  unifies a b | a == b = return nullSubst
  unifies (TPlus a) b = unifiesPartial a b
  unifies b (TPlus a) = unifiesPartial a b
  unifies (TVar v) t = bind v t
  unifies t (TVar v) = bind v t
  unifies (TApp t1 t2) (TApp t3 t4) = unifyMany (t1, t3) (t2, t4)
  unifies t1 t2 = throwErrorV $ UnificationFail (pretty t1) (pretty t2) 

instance PartialUnify Void where
  unifiesPartial = absurd

instance PartialUnify Choice where
  unifiesPartial t1 t2 = return nullSubst

unifyMany :: (Substituable a a, Unifiable a) => (a, a) -> (a, a) -> Solve (Subst a)
unifyMany (t1, t1') (t2, t2') = do
  s1 <- unifies t1 t1'
  s2 <- unifies (apply s1 t2) (apply s1 t2')
  return $ s2 `compose` s1

runUnify :: [Union] -> ExceptLog (Variational -> Type)
runUnify unions = do
  tell " ========= start solving =========\n\n "
  sub <- unionSolve (nullSubst, unions)
  let makeAllPlains = vtoT
  let sub' = makeAllPlains <$> sub
  return (apply sub' . makeAllPlains)

solver :: [(Pred Type, Location)] -> Solve [Pred Type]
solver ps = do
  preds <- classSolve $ ClassSolver [] ps
  tell $ "found preds : " ++ prettyL preds ++ "\n"
  return preds

vtoT :: Variational -> Solve Type
vtoT = \case
  TVar t -> return $ TVar t
  TCon t k -> return $ TCon t k
  TApp a b -> return $ TApp (vtoT a) (vtoT b)
  TPlus (Dim s ts) -> 
    let ts' = vtoT <$> ts in foldNE ts' keepIfEq
    where
      keepIfEq :: (Pretty a, Eq a) -> Solve a
      keepIfEq t1 t2 | t1 == t1 = return t1
      keepIfEq a b = throwError $ UnificationFail (pretty a) (pretty b)

unionSolve :: Unifier -> Solve VSubst
unionSolve (su, cs) =
  case cs of
    [] -> return su
    Union (t1, t2, loc) : cs1 -> do
      --tell $ "unify : " ++ pretty t1 ++ " and " ++ pretty t2 ++ "\n"
      su1 <- unifies t1 t2 `withErrorLoc` loc
      --tell $ "found : " ++ pretty su1 ++ "\n"
      unionSolve (su1 `compose` su, apply su1 cs1)

data ClassSolver = ClassSolver{found :: [Pred Type], remain :: [(Pred Type, Location)]};

classSolve :: ClassSolver -> Solve [Pred Type]
classSolve = \case
  ClassSolver founds [] -> return founds
  ClassSolver founds ((p, loc):ps) -> do
    preds <- solvePred p `withErrorLoc` loc
    classSolve (ClassSolver (founds ++ preds) ps) `withErrorLoc` loc

solvePred :: Pred Type -> Solve [Pred Type]
solvePred (IsIn n t) = do
  ClassEnv cenv <- ask
  case Map.lookup n cenv of
    Nothing -> throwErrorV $ UnknownClass n
    Just cls -> isIn n cls t

isIn :: String -> Class -> Type -> Solve [Pred Type]
isIn cname (m, insts) e = do
  tell $ "trying to satisfy : " ++ cname ++ " " ++ show e ++ " " ++ show insts ++ "\n"
  case e of
    TVar t -> return [IsIn cname $ TVar t]
    t -> satisfyInsts (IsIn cname t) insts

satisfyInsts :: Pred Type -> [Inst] -> Solve [Pred Type]
satisfyInsts (IsIn c t) [] = throwErrorV $ NotInClass c t
satisfyInsts s [i] = satisfyInst s i
satisfyInsts s (i:is) = satisfyInst s i `catchError` \e -> satisfyInsts s is

satisfyInst :: Pred Type -> Inst -> Solve [Pred Type]
satisfyInst (IsIn c t) q@(Qual ps (IsIn _ t')) = do
  s <- unifies t' t `catchError` const (throwErrorV $ NotInClass c t)
  return $ apply s ps

bind :: (Parametrized a, Eq a, Substitute (TypeF a)) => TVar -> TypeF a -> Solve (Subst (TypeF a))
bind a t | t == TVar a = return nullSubst
         | occursCheck a t = throwErrorV $ InfiniteType (TVar $ TV "not now" Star) 
         | otherwise = return $ Map.singleton a t

occursCheck :: Parametrized a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

unifyPred :: Pred Variational -> Pred Variational -> Solve VSubst
unifyPred (IsIn i t) (IsIn i' t') | i == i' = unifies t t'

mkPlain :: Variational -> ExceptLog Type
mkPlain t = let (Just t') = asType t in return t'
