{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveAnyClass #-}

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
import Data.Maybe
import Control.Arrow
import Subst
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub, find, length, intercalate)
import Pretty
import RecursionSchemes
import Data.Tuple
import Error
import Data.List (intersperse, elemIndex)
import Control.Applicative 

{-

dans allEq je dis oui si y en a une comme ça.
dans la génération du BitVec je fais pareil.
quand je renvoie le allReplace, j'enlève toutes les variables potetielles comme ça.
-}

newtype Union = Union (Type, Type, Location) deriving Show;

data Specialization = Spec{candidate :: Type, argType :: Type, retType :: Type, eqs :: Set.Set TVar, cand' :: Type};
data Reconcile = Reconcile ArrowType [Specialization] Location
type Constraints = ([Union], [(Pred, Location)], [Reconcile], [FreeVars])
type FreeVars = TVar;

instance Pretty Union where
  pretty (Union (a, b, _)) = pretty a ++ " should unify with : " ++ pretty b ++ "\n"

instance Pretty Reconcile where
  pretty (Reconcile c tps _) = prettyArr c ++ " rec with (" ++ mconcat (intersperse "," (prettySpec <$> tps)) ++ ")"
    where 
      prettySpec (Spec cand arg ret _ pat) = "(" ++ pretty cand ++ " @ " ++ pretty arg ++ " -> " ++ pretty ret ++ " pat : [" ++ pretty pat ++ "])"
      prettyArr (ArrowType a b) = pretty a ++ " -> " ++ pretty b

instance Pretty (Pred, Location) where
  pretty (a, b) = pretty a

instance Pretty Constraints where
  pretty (a, b, c, d) = prettyL a ++ " => " ++ prettyL b ++ "[" ++ prettyL c ++ "]\n norefine : " ++ prettyL b ++ "\n"

getEqConstraint :: Type -> (TVar, Type) -> Maybe TVar
getEqConstraint tp (v, TVar tv) =
  let (t :+: ts) = unapply tp in
  let t' = foldr TApp t ts in 
  let tvars = (ftv t) in 
  if v `Set.member` tvars && tv `Set.member` tvars
    then Just tv
    else Nothing
isEqConstraint _ _ = Nothing

instance Substituable (NonEmpty Type) where 
  apply s = fmap $ apply s
  ftv _ = Set.empty

instance Substituable Specialization where
  apply s (Spec cand arg ret eqs cand') = Spec (apply s cand) (apply s arg) (apply s ret) (eqs `Set.union` eqs') (apply s cand')
    where eqs' = Set.fromList $ catMaybes (getEqConstraint cand <$> Map.toList s) 
  ftv = const Set.empty

instance Substituable Reconcile where
  apply s (Reconcile c ts l) = Reconcile (apply s c) (apply s <$> ts) l
  ftv _ = Set.empty

union :: Location -> (Type, Type) -> Constraints
union loc (a, b) = ([Union (a, b, loc)], [], [], [])

predicate :: [Pred] -> Location -> Constraints
predicate p l = ([], (,l) <$> p, [], [])

-- reconcile R a -> b with [D a -> b, W c -> d]
reconcilie :: Type -> Type -> [Specialization] -> Location -> Constraints
reconcilie a b tps l = ([], [], [Reconcile (ArrowType a b) tps l], [])

free :: TVar -> Constraints
free a = ([], [], [], [a])

instance Substituable Union where
  apply s (Union (a, b, c)) = Union (apply s a, apply s b, c)
  ftv (Union (a, b, _)) = ftv a `Set.union` ftv b

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
  | ConstructorsNotMatching Type Type
  | CannotRefine TVar
  | CouldNotGeneralize [Type]
  | CouldNotReconcile ArrowType ArrowType
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
    ConstructorsNotMatching t t' -> "Patterns do not match : " ++ pretty t ++ " with "++ pretty t'
    CannotRefine tv -> "Cannot refine : " ++ pretty tv
    CouldNotGeneralize tps -> "could not generalize the types : " ++ prettyL tps
    CouldNotReconcile a b-> "Could not reconcile : " ++ pretty (asType a) ++ " and " ++ pretty (asType b)
    ConstraintsNotMatching t -> "Infered constraints not found in definition : " ++ prettyL t

type ExceptLog a = ExceptT TypeError (Writer String) a;
type Reconciling a = StateT InferState (ExceptT TypeError (Writer String)) a;

type Infer = ReaderT Env (StateT InferState (Except TypeError))
type InferCons = WriterT Constraints Infer

type Solve a = (ReaderT ClassEnv (ExceptT TypeError (Writer String)) a)

newtype InferState = InferState {count :: Int}

initInfer :: InferState
initInfer = InferState { count = 0 }

asSolve :: Unifying a -> Solve a
asSolve a = ReaderT $ const (evalStateT a Set.empty) 

asReconciling :: ExceptLog a -> Reconciling a
asReconciling a = StateT $ \i -> fmap (,i) a

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

-- TODO merge these two pls.
inferExpr :: ClassEnv -> Env -> Expr -> ExceptLog (Scheme, TExpr)
inferExpr cenv env ex = case runInfer env (runWriterT $ infer ex) of
  Left err -> throwError err
  Right (tyBefore, (unions, preds, dims, norefs)) -> do
    let (_, _, Qual _ ty) = ann tyBefore
    tell $ "things : " ++ prettyL dims
    --tell $ "found : "++ pretty ty ++ "\n"
    tell $ "with type : " ++ pretty ty ++ " solve => \n"
    --tell $ "constraints : " ++ pretty preds ++ " go \n"
    subst <- runUnify unions dims norefs ty
    tell $ "found subst : " ++ pretty subst
    --preds' <- sequence (_vtoT . apply subst <$> preds)
    preds'' <- runSolve cenv (first (apply subst) <$>  preds)
    let ty' = apply subst ty
    let tyBefore' = applyAnn (apply subst) tyBefore
    --tell $ "before : " ++ pretty (apply subst ty') ++ "\n" ++ pretty (apply subst ty) ++ "\n"
    --let (b, s) = closeOver (apply subst ty) (apply subst preds) $ apply subst ty'
    --tell $ "after CO : " ++ pretty b ++ "\n" ++ pretty s ++ "\n"
    return (closeOver ty' preds'' tyBefore')

-- waaaay too complicated. TODO: pls simplify this.
inferExprT :: ClassEnv -> Env -> Expr -> Scheme -> ExceptLog (Scheme, TExpr)
inferExprT cenv env ex tp = case runInfer env (runWriterT $ inferEq ex tp) of
  Left err -> throwError err
  Right ((texp, tpSub, expected), (unions, preds, things, norefs)) -> do
    tell $ "things : " ++ prettyL things ++ "\n"
    tell $ "doing : " ++ prettyL unions ++ "\n"
    let (_, _, Qual _ found) = ann texp
    subst <- runUnify unions things norefs found
    preds <- runSolve cenv $ first (apply subst) <$> preds
    let Forall _ (Qual expectedPreds _) = tp
    tell $ "found : " ++ pretty (apply subst found) ++ "\n"
    tell $ "expected : " ++ pretty (apply subst expected) ++ "\n"
    tell $ "tpsub : " ++ pretty tpSub ++ "\n"
    allReplaces <- checkStrict (apply subst found) (apply subst expected)
    tell $ "all rep" ++ prettyL allReplaces ++ "\n"
    tell $ "subst is : " ++ pretty subst ++ "\n"
    tell $ "checking preds : " ++ prettyL (apply (Map.fromList allReplaces) preds) ++ " vs " ++ prettyL (apply subst (apply tpSub expectedPreds)) ++ "\n"
    checkPreds (apply (Map.fromList allReplaces) preds) $ apply subst (apply tpSub expectedPreds)
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
freshType = do
  tv <- flip TV Star <$> freshName
  tell $ free tv
  return $ TVar tv

freshQual :: InferCons (Qual Type)
freshQual = Qual [] <$> fresh

noConstraints :: Constraints
noConstraints = ([], [], [], [])

lookupEnv :: Name -> InferCons (Subst, Qual Type)
lookupEnv x = do
  (TypeEnv env) <- ask
  case Map.lookup x env of
    Just s -> instantiate s
    Nothing -> throwErrorV $ UnboundVariable $ show x

inferEq :: Expr -> Scheme -> InferCons (TExpr, Subst, Type)
inferEq e t0 = do
  (s, Qual q t1) <- instantiate t0
  t2 <- infer e
  return (t2, s, t1)

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
      tell $ reconcilie sourceType ret trets l

      let pats' = fmap return <$> pats
      retExpr <- sequence $ Case sourceT pats'
      return (In $ (l, nullSubst, Qual [] ret) :< retExpr)

    (l, Lam pat) -> do
      (Spec _ tin tout _ _, pat') <- inferPat l pat
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
            (sub, Qual p t) <- lookupEnv x
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
    let tret = getReturn tname
    tell (loc `union` (foldr mkArr tret subTypes, tname))
    return (tret, mconcat varsTypes)
decomposePattern loc (Raw name) = do
    tret <- freshType
    return (tret, return (name, tret))

inferPat :: Location -> PatternT (InferCons TExpr) -> InferCons (Specialization, PatternT TExpr)
inferPat loc (PatternT pat exp) = do
  -- tp = in type, eg: Bool
    (tp, vars) <- decomposePattern loc pat
    tret <- foldl (flip inEnv) exp vars
    cand <- makeCandidate tp
    return (Spec cand tp (getTp tret) Set.empty tp, PatternT pat tret)

makeCandidate :: Type -> InferCons Type
makeCandidate = makeCandidate' True
  where 
    makeCandidate' isLeft = \case
      tp@(TCon t _) -> if isLeft then return tp else fresh
      TVar t -> return (TVar t)
      TApp a b -> do
        a' <- makeCandidate' isLeft a
        b' <- makeCandidate' False b
        return $ TApp a' b'

  
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
      remaining = (f `Set.difference` e) in do
        tell $ prettyL (Set.toList f)
        tell $ prettyL (Set.toList e)
        if Set.empty == remaining
          then return ()
          else throwErrorV $ ConstraintsNotMatching $ Set.toList remaining

type Unifying a = StateT (Set.Set TVar) (ExceptT TypeError (Writer String)) a
type Unifier = (Subst, [Union])

emptyUnifier :: Unifier
emptyUnifier = (nullSubst, [])

unifies :: Type -> Type -> Unifying Subst
unifies t1 t2 | t1 == t2 = return nullSubst
unifies (TVar v) t = bind v t
unifies t (TVar v) = bind v t
unifies (TApp t1 t2) (TApp t3 t4) = unifyMany (t1, t3) (t2, t4)
unifies t1 t2 = throwErrorV $ UnificationFail t1 t2

unifyMany :: (Type, Type) -> (Type, Type) -> Unifying Subst
unifyMany (t1, t1') (t2, t2') = do
  s1 <- unifies t1 t1'
  s2 <- unifies (apply s1 t2) (apply s1 t2')
  return $ s2 `compose` s1

runUnify :: [Union] -> [Reconcile] -> [FreeVars] -> Type -> ExceptLog Subst
runUnify u r f t = evalStateT (unifyTop u r t) $ Set.fromList f

unifyTop :: [Union] -> [Reconcile] -> Type -> Unifying Subst
unifyTop [] [] _ = return nullSubst
unifyTop [] (r:rs) base = do
  tell " ========= start reconciling =========\n\n "
  tell $ "reconciling : \n" ++ pretty r ++ "\n"
  tell $ "after : " ++ prettyL rs ++ "\n"
  f <- gets Set.toList
  tell $ "bfore filtering : " ++ prettyL f ++ "\n"
  modify $ Set.filter (isForall base r)
  f <- gets Set.toList
  tell $ "after filtering : " ++ prettyL f ++ "\n"
  sub <- runReconcile r
  tell $ "sus is then : " ++ pretty sub
  sub' <- unifyTop [] (apply sub rs) base
  tell $ "sub' is then : " ++ pretty sub'
  return (sub' `compose` sub)
unifyTop unions recs t = do
  tell " ========= start solving =========\n\n "
  dr <- Set.toAscList <$> get
  tell $ "free are : " ++ prettyL dr
  tell $ "unify : " ++ prettyL unions ++ "\n"
  sub <- unionSolve (nullSubst, unions)
  tell $ "ty would be " ++ pretty (apply sub t)
  sub' <- unifyTop [] (apply sub recs) (apply sub t)
  return (sub' `compose` sub)


-- donc je veux regarder pour chaque tvar dans le constructeur ou les types de retour.
--  - si elle apparait dans le type de retour.
--  - si elle apparait dans un autre argument (entrée ou sortie.)

isForall :: Type -> Reconcile -> TVar -> Bool
isForall base (Reconcile _ specs _) tv = (tv `appearsInType` base + sum (countAppearances tv <$> specs)) <= 1

countAppearances :: TVar -> Specialization -> Int
countAppearances tv (Spec _ arg ret _ p) = tv `appearsInType` arg + tv `appearsInType` ret 

appearsInType :: TVar -> Type -> Int
appearsInType tv tp = fromEnum (tv `Set.member` ftv tp)  


solver :: [(Pred, Location)] -> Solve [Pred]
solver ps = do
  preds <- classSolve $ ClassSolver [] ps
  tell $ "found preds : " ++ prettyL preds ++ "\n"
  return preds

sequenceFirst :: Functor f => (f a, b) -> f (a, b)
sequenceFirst (a, b) = (,b) <$> a

unionSolve :: Unifier -> Unifying Subst
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

bind :: TVar -> Type -> Unifying Subst
bind a t | t == TVar a = return nullSubst
         | occursCheck a t = throwErrorV $ InfiniteType t 
         | otherwise = return $ Map.singleton a t

occursCheck :: TVar -> Type -> Bool
occursCheck a t = a `Set.member` ftv t

unifyPred :: Pred -> Pred -> Unifying Subst
unifyPred (IsIn i t) (IsIn i' t') | i == i' = unifies t t'

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map Prelude.head x) : transpose (map tail x)
-- TODO renommer les subst un peu partout
runReconcile :: Reconcile -> Unifying Subst
runReconcile (Reconcile (ArrowType from to) ts l) = do
  --tell $ "merge : " ++ prettyL ts ++ "\n"
  (ArrowType from' to', sub) <- mergeSpecs ts `withErrorLoc` l
  tell $ "\nfound : " ++ pretty from' ++ " -> " ++ pretty to' ++ "\n"
  subst <- unifies from' from
  subst' <- unifies (apply subst to) (apply subst to')
  return (subst' `compose` subst `compose` sub) 

data Columns f a = Columns{cand :: a, cols :: f a} deriving (Show, Ord, Eq, Functor, Applicative, Foldable, Traversable)

instance Pretty a => Pretty (Columns [] a) where
  pretty (Columns cand cols) = "(" ++ pretty cand ++ " | " ++ mconcat (intersperse ", " $ pretty <$> cols) ++ ")"

withCandidate :: [Type] -> Columns [] Type
withCandidate (a:as) = Columns a (a:as)

unifyAll :: [Type] -> Unifying (Type, Subst)
unifyAll (a:as) = foldM combine (a, nullSubst) as
  where 
    combine :: (Type, Subst) -> Type -> Unifying (Type, Subst)
    combine (a, sb) b = do
      sub <- unifies a $ apply sb b
      return (apply sub b, sub `compose` sb)

checkGeneralization :: Type -> Type -> Unifying ()
checkGeneralization t t' = do
  unifies t t'
  return ()

mergeSpecs :: [Specialization] -> Unifying (ArrowType, Subst)
mergeSpecs specs = do
  (arg, sub) <- unifyAll (candidate <$> specs)
  let specs' = specs
  mapM_ (checkGeneralization arg) (argType <$> specs')
  -- type de retour, [cases] -> liste de aaaah je peux map le toList ptet ?
  let colsArgs = treeToList (getMinimalType (Columns arg (argType <$> specs')))
  tell $ "first step : " ++ pretty (Columns arg (argType <$> specs')) ++ "\n"
  tell $ "second step : " ++ pretty (getMinimalType (Columns arg (argType <$> specs'))) ++ "\n"
  tell $ "first step : " ++ prettyL (treeToList (getMinimalType (Columns arg (argType <$> specs')))) ++ "\n"
  tell $ "args : " ++ pretty arg ++ "\n"
  tell $ "first : " ++ pretty (Columns arg (argType <$> specs')) ++ "\n"
  tell $ "snd : " ++ pretty (asTree <$> Columns arg (argType <$> specs')) ++ "\n"
  let colsRets = treeToList (getMinimalType $ withCandidate (retType <$> specs'))
  tell $ "args : " ++ prettyL colsArgs ++ "\n"
  tell $ "rets : " ++ prettyL colsRets ++ "\n"
  retSubst <- unifyRets colsArgs colsRets []
  tell $ "replace final is : " ++ prettyL retSubst ++ "\n"
  let retFinal = replaceAll retSubst $ Prelude.head (retType <$> specs')
  tell $ "final ret is : " ++ pretty retFinal ++ "\n"
  return (ArrowType arg retFinal, sub)

unifyRets :: [Columns [] Type] -> [Columns [] Type] -> Replacements -> Unifying Replacements
unifyRets values [] rep = return rep
unifyRets values (x:xs) rep = do
  newRep <- unifyRet values x
  orderedRep <- reorderReplaces newRep
  unifyRets values xs (rep ++ orderedRep)

reorderReplaces :: Replacements -> Unifying Replacements
reorderReplaces = mapM reorderReplace
  where 
    reorderReplace (a, b) = do
      isFa <- removeIfFree a
      isFb <- removeIfFree b
      if isFb && (not isFa) then return (b, a) else return (a, b)

removeIfFree :: Type -> Unifying Bool
removeIfFree t@(TVar tv) = do
  isF <- isFree t
  if isF
    then do
      modify $ flip Set.difference (Set.singleton tv)
      return True
    else return False
removeIfFree _ = return False

isFree :: Type -> Unifying Bool
isFree = \case
  TVar tv -> gets $ Set.member tv
  _ -> return False

-- TODO : verify that this is not fucked up sometimes. quoi que je 
allEqsOrFree :: [Type] -> Unifying Bool
allEqsOrFree xs = do
  tell $ "filtered : " ++ prettyL xs ++ "\n"
  xs' <- filterM (fmap not . isFree) xs
  tell $ "filtered : " ++ prettyL xs' ++ "\n"
  return $ Set.size (Set.fromList xs') <= 1

allEqs :: [Type] -> Bool
allEqs xs = Set.size (Set.fromList xs) <= 1

findCorrespondingCol :: Columns [] Type -> Columns [] Type -> Maybe (Type, Type)
findCorrespondingCol (Columns retC retCols) (Columns argC argCols) = 
  if argCols == retCols 
    then Just (retC, argC) 
    else Nothing

-- TODO : clean up this mess
unifyRet :: [Columns [] Type] -> Columns [] Type -> Unifying [(Type, Type)]
unifyRet finals ret@(Columns cd retCols@(r:rs)) = do
  let getCandAt x = cand (finals !! (x - 1))
  let getArgsAt x = cols (finals !! (x - 1))
  tell $ "ret Column "  ++ prettyL retCols ++ "\n"
  tell $ "args Columns "  ++ prettyL finals ++ "\n"
  --tell $ "same : " ++ show same ++ "\n"
  if allEqs retCols
    then do
      modify $ flip Set.difference (Set.fromList $ retCols >>= Set.toList . ftv)
      tell $ "unify all these " ++ prettyL ((r,) <$> rs) ++ "\n"
      return ((r,) <$> rs)
    else do
      map <- asMap finals ret 
      tell $ (show map) 
      tell $ show (findInCombinations (length finals) (isValid map))
      case findInCombinations (length finals) (isValid map) of
        Just [(x:xs)] -> do
          -- remove the actual values from the free list. not cand then !!
          let newSubst = ((cd, cand (finals !! (x - 1))) : ((getCandAt x,) . getCandAt <$> xs))
          tell $ "new subst tvars : " ++ prettyL (x:xs >>= (Set.toList . ftv . getArgsAt)) ++ "\n"
          dr <- gets Set.toAscList
          tell $ "free were : " ++ prettyL dr ++ "\n"
          tell $ "args : " ++ prettyL (x:xs >>= (Set.toList . ftv . getArgsAt)) ++ "\n"
          --modify $ flip Set.difference (Set.fromList (x:xs >>= (Set.toList . ftv . getArgsAt)))
          dr <- gets Set.toAscList
          tell $ "free became : " ++ prettyL dr ++ "\n"
          tell $ "unify all these " ++ prettyL newSubst ++ "\n"
          return newSubst
        _ -> do
          same <- allEqsOrFree retCols 
          if same 
            then do
              tell $ "unify all these " ++ prettyL ((r,) <$> rs) ++ "\n"
              return ((r,) <$> rs)
            else throwErrorV $ CouldNotGeneralize retCols


type Generalization = Map.Map Type TVar
type TwoSided = (Generalization, Generalization)

instance Pretty TwoSided where
  pretty (a, b) = "(" ++ prettyL (Map.toList a)  ++ ", " ++ prettyL (Map.toList b) ++ ")"

applyG :: Replacable a => Generalization -> a -> a
applyG g a = replaceAll (Map.toList $ Map.map TVar g) a

applyBoth :: TwoSided -> ArrowType -> ArrowType
applyBoth (a, b) (ArrowType a2 b2) = ArrowType (applyG a a2) (applyG b b2)

mergeG :: [Generalization] -> Generalization
mergeG gens = (foldr (flip Map.union) Map.empty) gens

freshRec :: Reconciling String
freshRec = do
  s <- get
  put InferState{count = count s + 1}
  return ("'" ++ letters !! count s)

type BitVec = [Bool]

asBinary :: Columns [] Type -> Columns [] Type -> Unifying BitVec
asBinary (Columns retC retCols) (Columns argC argCols) = 
  cmpType `mapM` zip retCols argCols
  where 
    cmpType (a, b) = do
      isFa <- isFree a
      return (isFa || a == b)

asMap :: [Columns [] Type] -> Columns [] Type -> Unifying (Map.Map Int BitVec)
asMap finals ret = Map.fromList <$> zip [1..] <$> mapM (asBinary ret) finals

bAnd :: BitVec -> BitVec -> BitVec
bAnd a b = getZipList $ (||) <$> ZipList a <*> ZipList b

foldne :: (a -> a -> a) -> [a] -> a
foldne f (x:xs) = foldr f x xs

isValid :: Map.Map Int BitVec -> [Int] -> Bool
isValid m = fmap (m Map.!) >>> foldne bAnd >>> all id


fac :: Int -> Int
fac n = foldr (*) 1 [1..n]

splitToNextSize :: Int -> Int -> [a] -> ([a], [a])
splitToNextSize n k l = 
  let next = (fac n) `div` (fac k * fac (n - k)) in splitAt next l

combinations :: [Int] -> [[Int]]
combinations start = take (2 ^ length start - 1) (combinations' $ fmap (:[]) start)
  where 
    combinations' a = a ++ combinations' (next a) 
    combine b x = (x:) <$> filter ((x <) . Prelude.head) b
    next b = start >>= combine b

combinationsSeparated :: Int -> [[[Int]]]
combinationsSeparated n =
  let l = combinations [1..n] in
    nextCombinations 1 l
  where  
    nextCombinations :: Int -> [[Int]] -> [[[Int]]]
    nextCombinations k l | k == n + 1 = []
    nextCombinations k l =
      let (now, remain) = splitToNextSize n k l in
        now : nextCombinations (k + 1) remain

findInCombinations :: Int -> ([Int] -> Bool) -> Maybe [[Int]]
findInCombinations size f =
  let result = filter f <$> combinationsSeparated size in
    findFirst (/=[]) result

findFirst :: (Eq a) => (a -> Bool) -> [a] -> Maybe a
findFirst f [] = Nothing
findFirst f (x:xs) | f x = Just x
findFirst f (x:xs) = findFirst f xs
