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
import Data.List (nub, find, length, intercalate, intersperse, elemIndex)
import Pretty
import RecursionSchemes
import Data.Tuple
import Error
import Control.Applicative

{-

dans allEq je dis oui si y en a une comme ça.
dans la génération du BitVec je fais pareil.
quand je renvoie le allReplace, j'enlève toutes les variables potetielles comme ça.
-}

newtype Union = Union (Type, Type, Location) deriving Show;


-- candidate tells what the constructor expected (structure and TCon)
-- argType tells us what the final arg type was (cand + branch)
-- retType is the final ret type (cand + branch + transformation)
data Specialization = Spec{candidate :: Type, argType :: Type, retType :: Type};
data Reconcile = Reconcile ArrowType [Specialization] Location Name
type Constraints = ([Union], [(Pred, Location)], [Reconcile], [FreeVars], [String])
type FreeVars = TVar;

instance Pretty Union where
  pretty (Union (a, b, _)) = pretty a ++ " should unify with : " ++ pretty b ++ "\n"

instance Pretty Reconcile where
  pretty (Reconcile c tps _ name) = "rec(" ++ name ++ ") : " ++ prettyArr c ++ " with (" ++ mconcat (intersperse "," (prettySpec <$> tps)) ++ ")"
    where
      prettySpec (Spec cand arg ret) = "(" ++ pretty cand ++ " @ " ++ pretty arg ++ " -> " ++ pretty ret
      prettyArr (ArrowType a b) = pretty a ++ " -> " ++ pretty b

instance Pretty (Pred, Location) where
  pretty (a, b) = pretty a

instance Pretty Constraints where
  pretty (a, b, c, d, l) = prettyL a ++ " => " ++ prettyL b ++ "[" ++ prettyL c ++ "]\n norefine : " ++ prettyL b ++ "\n"

instance Substituable (NonEmpty Type) where
  apply s = fmap $ apply s
  ftv _ = Set.empty

instance Substituable Specialization where
  apply s (Spec cand arg ret) = Spec cand (apply s arg) (apply s ret)
  ftv = const Set.empty

instance Substituable Reconcile where
  apply s (Reconcile c ts l name) = Reconcile (apply s c) (apply s <$> ts) l name
  ftv _ = Set.empty

union :: Location -> (Type, Type) -> Constraints
union loc (a, b) = ([Union (a, b, loc)], [], [], [], [])

lunion :: Location -> (Type, Type) -> Union
lunion loc (a, b) = Union (a, b, loc)

predicate :: [Pred] -> Location -> Constraints
predicate p l = ([], (,l) <$> p, [], [], [])

-- reconcile R a -> b with [D a -> b, W c -> d]
reconcilie :: Type -> Type -> [Specialization] -> Location -> Name -> Constraints
reconcilie a b (t:tps) l n = ([], [], [Reconcile (ArrowType a b) (t:tps) l n], [], [])

free :: TVar -> Constraints
free a = ([], [], [], [a], [])

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
  | WrongPattern Type Type
  | CaseDependency Name Name
  | WrongBranchType Type Type
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
    WrongPattern expected actual -> "Wrong subpattern type : exptected " ++ pretty expected ++ " got " ++ pretty actual
    WrongBranchType cand final -> "Wrong branch Type, cand : " ++ pretty cand ++ " final : " ++ pretty final 
    CaseDependency n1 n2 -> "Dependency between " ++ n1 ++ " and " ++ n2

type ExceptLog a = ExceptT TypeError (Writer String) a;

type Infer = ReaderT Env (StateT InferState (Except TypeError))
type InferCons = WriterT Constraints Infer

type Solve a = (ReaderT ClassEnv (ExceptT TypeError (Writer String)) a)

newtype InferState = InferState {count :: Int}

initInfer :: InferState
initInfer = InferState { count = 0 }

asSolve :: Unifying a -> Solve a
asSolve a = ReaderT $ const (evalStateT a 0)

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
  Right (tyBefore, (unions, preds, dims, norefs, logs)) -> do
    tell $ intercalate "\n" logs
    let (_, _, Qual _ ty) = ann tyBefore
    tell $ "things : " ++ prettyL dims
    --tell $ "found : "++ pretty ty ++ "\n"
    tell $ "with type : " ++ pretty ty ++ " solve => \n"
    --tell $ "constraints : " ++ pretty preds ++ " go \n"
    subst <- runUnify unions dims norefs ty
    --tell $ "found subst : " ++ pretty subst
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
  Right ((texp, tpSub, expected), (unions, preds, things, norefs, logs)) -> do
    tell $ intercalate "\n" logs
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

freshVar :: InferCons TVar
freshVar = flip TV Star <$> freshName

freshType :: InferCons Type
freshType = do
  tv <- freshVar
  tell $ free tv
  return $ TVar tv

freshQual :: InferCons (Qual Type)
freshQual = Qual [] <$> fresh

noConstraints :: Constraints
noConstraints = ([], [], [], [], [])

inferLog :: String -> InferCons ()
inferLog s = tell ([], [], [], [], [s])

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
      name <- freshName
      tell $ reconcilie sourceType ret trets l name

      let pats' = fmap return <$> pats
      retExpr <- sequence $ Case sourceT pats'
      return (In $ (l, nullSubst, Qual [] ret) :< retExpr)

    (l, Lam pat) -> do
      (Spec _ tin tout, pat') <- inferPat l pat
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

typeOf :: String -> InferCons Type
typeOf x = do
    (sub, Qual p t) <- lookupEnv x
    return t

decomposePattern :: Location -> Pattern -> InferCons (Type, [(String, Type)])
decomposePattern loc (Pattern name vars) = do
    subPats <- traverse (decomposePattern loc) vars
    let (subTypes, varsTypes) = unzip subPats
    tname <- typeOf name
    argsSubst <- Map.fromList . mconcat <$> traverse (getCandSubst loc) (zip (uncurryType tname) subTypes )
    let tret = apply argsSubst $ getReturn tname
    tell (loc `union` (foldr mkArr tret subTypes, tname))
    inferLog $ "substs are : " ++ pretty argsSubst
    inferLog $ "tret goes from " ++ pretty(getReturn tname) ++ " to " ++ pretty tret
    return (tret, mconcat varsTypes)
decomposePattern loc (Raw name) = do
    tret <- freshType
    return (tret, [(name, tret)])

removeLeft :: Location -> Type -> InferCons Type
removeLeft loc = \case
  TApp a b -> (`TApp` b) <$> removeLeft loc a
  TCon t c ->  do
    rep <- freshType
    tell $ union loc (rep, TCon t c)
    return rep
  t -> return t

-- given a the expected arg and the type of the sub pattern, tells what to do with the expected arg.
getCandSubst :: Location -> (Type, Type) -> InferCons [(TVar, Type)]
getCandSubst loc (expected, actual) = do
  actual' <- removeLeft loc actual
  case (expected, actual') of
    (TVar e, TVar a) -> return [(e, actual')]
    (_, TVar _) -> return []

    (TVar e, TApp _ _) -> return [(e, actual')]
    (TCon _ _, TApp _ _) -> throwError $ CompilerError (WrongPattern expected actual') []
    (TApp a b, TApp a' b') -> (++) <$> getCandSubst loc (a, a') <*> getCandSubst loc (b, b')
    
    (TCon c k, TCon d k') -> if c == d then return [] else throwError $ CompilerError (WrongPattern expected actual') []
    (TApp a b, TCon c k) -> throwError $ CompilerError (WrongPattern expected actual') []
    (TVar e, TCon c k) -> return [(e, actual')]


inferPat :: Location -> PatternT (InferCons TExpr) -> InferCons (Specialization, PatternT TExpr)
inferPat loc (PatternT pat exp) = do
  -- tp = in type, eg: Bool
    (tp, vars) <- decomposePattern loc pat
    inferLog $ "pat is : " ++ pretty pat
    inferLog $ "decomposed is : " ++ pretty tp
    
    tret <- foldl (flip inEnv) exp vars
    return (Spec tp tp (getTp tret), PatternT pat tret)


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


type ReconcilingGraph = Map.Map String (Reconcile, [String]);
type Unifying a = StateT Int (ExceptT TypeError (Writer String)) a
type Unifier = (Subst, [Union])

instance Pretty (Reconcile, [String]) where
  pretty (r, str) = intercalate ", " str

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
runUnify u r f t = evalStateT (unifyTop u r t) 0

data DType
  = DVar TVar
  | Or [DType]
  | DCon String Kind
  | DApp Type Type deriving (Show, Eq, Ord)

unifyTop :: [Union] -> [Reconcile] -> Type -> Unifying Subst
unifyTop [] recs base = do
  tell " ========= start reconciling =========\n\n "
  tell $ "after : " ++ prettyL recs ++ "\n"
  tell $ "base : " ++ pretty base ++ "\n"
  checkReconcilingGraph recs
  reconcilieTop recs base
unifyTop unions recs t = do
  tell " ========= start solving =========\n\n "
  --tell $ "unify : " ++ prettyL unions ++ "\n"
  sub <- unionSolve (nullSubst, unions)
  --tell $ "ty would be " ++ pretty (apply sub t)
  sub' <- unifyTop [] (apply sub recs) (apply sub t)
  return (sub' `compose` sub)

checkReconcilingGraph :: [Reconcile] -> Unifying ()
checkReconcilingGraph s = sequence_ (hasCommonTVar <$> s <*> s )

hasCommonTVar :: Reconcile -> Reconcile -> Unifying ()
hasCommonTVar (Reconcile (ArrowType a b) _ _ name) r@(Reconcile _ _ _ name2) = 
  let tvs = ftv (mkArr a b) in when (any (`appearsInRec` r) tvs) (throwError $ CompilerError (CaseDependency name name2) [])

appearsInRec :: TVar -> Reconcile -> Bool
appearsInRec tv (Reconcile arrow spec l _)  = any (appearsInSpec tv) spec

appearsInSpec :: TVar -> Specialization -> Bool
appearsInSpec tv (Spec cand arg ret) = appearsInType tv cand || appearsInType tv arg || appearsInType tv ret

appearsInType :: TVar -> Type -> Bool
appearsInType tv tp = tv `Set.member` ftv tp

reconcilieTop :: [Reconcile] -> Type -> Unifying Subst
reconcilieTop rs t = foldl compose nullSubst <$> traverse (runReconcile t) rs

getTVarTree :: [Type] -> Unifying (BTree TVar)
getTVarTree = fmap asTree >>> getMaximalStruct >>> traverse (fmap var . const freshRec)

-- TODO renommer les subst un peu partout
runReconcile :: Type -> Reconcile -> Unifying Subst
runReconcile base (Reconcile (ArrowType from to) ts l n) = do
  -- structs is the tree of tvars with the same maximum size as the argtypes so it can give a replacement for every leaf.
  structs <- getTVarTree (argType <$> ts)
  let candAndArgs = (candidate &&& argType) <$> ts
  --tell $ "tvar tree : " ++ pretty structs ++ "\n"
  -- vars that were replaced by types
  replacements <- join <$> sequence (getReplacements base structs <$> candAndArgs)
  --tell $ "replacements : " ++ prettyL replacements ++ "\n"
  -- vars that become equal to other vars.
  varsreplacements <- replacementsToVars <$> (compareReplacements <$> (join <$> sequence (getVarsReplacements structs <$> candAndArgs)))
  --tell $ "equalities : " ++ show ((pretty *** pretty) <$> varsreplacements) ++ "\n"
  sub0 <- unionSolve (nullSubst, lunion l <$> (replacements ++ varsreplacements))
  --tell $ "sub is : " ++ pretty sub0 ++ "\n"
  uncollapsables <- getMaximalStruct <$> sequence (getUncollapsableVars <$> candAndArgs)
  let structs' = apply sub0 $ tvTreeToType structs
  tell $ "new struct : " ++ pretty structs' ++"\n"
  tell $ "uncollapsables : " ++ show (pretty uncollapsables) ++ "\n"
  outTypes <- traverse (localReconcilie structs') ts
  return nullSubst

localReconcilie :: Type -> Specialization -> Unifying [Type]
localReconcilie inType (Spec cand arg ret) = do
  tell $ "cand : " ++  pretty cand ++ "     tp :  " ++ pretty inType ++ "\n"
  sub <- extractLocalSubst cand inType
  tell $ "found sub : " ++ show (pretty <$> sub) ++ "\n"
  let res = applyMany sub ret
  tell $ "rets are : " ++ show (pretty <$> res) ++ "\n"
  return []

applyHigherSub :: Bool -> (Type, Type) -> Type -> [Type]
applyHigherSub isLeft (from, to) source@(TApp _ _) | source == from = [to, from] -- just remove some useless cases where we replace the base type.
applyHigherSub False (from, to) source | source == from = [to, from] -- just remove some useless cases where we replace the base type.
applyHigherSub isLeft (from, to) (TApp a b) = TApp <$> applyHigherSub isLeft (from, to) a <*> applyHigherSub False (from, to) b
applyHigherSub _ _ s = [s]

applyMany :: [(Type, Type)] -> Type -> [Type]
applyMany [] t = [t]
applyMany (rep:reps) t = let others = applyMany reps t in (>>= applyHigherSub True rep) others

-- TODO trap in the ``compose` maybe, if incompatibilities in the substs (idk)
extractLocalSubst :: Type -> Type -> Unifying [(Type, Type)]
extractLocalSubst cand final = case (cand, final) of
  (TApp a b, TApp a' b') -> do
    sa <- extractLocalSubst a a'
    sb <- extractLocalSubst b b'
    return (sa ++ sb)
  -- ici j'ai un TApp qui correspond à une var donc je veux pouvoir transformer ce tapp en var
  -- /!\ INVERSER LA SUBST
  (TApp a b, TVar tv) -> return [(cand, final)]
  -- ici j'ai un tapp en candidat mais le type final est un TCon donc c'est incompatible -> voila
  (TApp a b, TCon tc k) -> throwErrorV $ WrongBranchType cand final

  (TVar tv, TVar tv') -> return [(cand, final)]
  -- ici je dis que la tvar dans le candidat peut être remplacée par le tapp (car elle le sera obligatoirement)
  (TVar tv, TApp a b) -> return [(cand, final)]
  -- ici j'ai en candidat une tvar du coup pareil que pour le tapp
  (TVar tv, TCon t k) -> return [(cand, final)]

  (TCon t k, TCon t' k') -> if t /= t' then throwErrorV $ WrongBranchType cand final else return []
  (TCon t k, TVar tv') -> return [(cand, final)]
  (TCon t k, TApp a b) -> throwErrorV $ WrongBranchType cand final -- psk c'est logique

compareReplacements :: [(BTree TVar, TVar, TVar)] -> [(BTree TVar, BTree TVar)]
compareReplacements lst = join (filterReplacements <$> lst <*> lst)
  where 
    filterReplacements (orig, cand, actual) (orig', cand', actual') =
      [(orig, orig') | orig /= orig' && cand /= cand' && actual == actual']


replacementsToVars :: [(BTree TVar, BTree TVar)] -> [(Type, Type)]
replacementsToVars = fmap (TVar *** tvTreeToType) <$> (=<<) replacementsToVars'
  where replacementsToVars' = \case
          (Leaf tv, x) -> [(tv, x)]
          (x, Leaf tv) -> [(tv, x)]
          (BTree (a, b), BTree (c, d)) -> replacementsToVars' (a, c) ++ replacementsToVars' (b, d)


-- detects structure mismatchs (eg : Int, f a is going to produce * * but f a, g b will produce *)
getUncollapsableVars :: (Type, Type) -> Unifying (BTree ())
getUncollapsableVars (candidate, actual) = case (candidate, actual) of
  (TApp a b, TApp a' b') -> do
    rep <- getUncollapsableVars (a, a')
    rep' <- getUncollapsableVars (b, b')
    case (rep, rep') of
      (Leaf () , Leaf ()) -> return $ Leaf ()
      _ -> return $ BTree (rep, rep')
  (t, TApp a b) -> do
    rep <- getUncollapsableVars (t, a)
    rep' <- getUncollapsableVars (t, b)
    return (BTree (rep, rep'))
  (_, _) -> return $ Leaf ()

getVarsReplacements :: BTree TVar -> (Type, Type) -> Unifying [(BTree TVar, TVar, TVar)]
getVarsReplacements replacement (candidate, actual) = case (replacement, candidate, actual) of
  (BTree (a, b), TApp a' b', TApp a'' b'') -> do
    rep <- getVarsReplacements a (a', a'')
    rep' <- getVarsReplacements b (b', b'')
    return (rep ++ rep')
  (t, TVar tv', TVar tv'') -> return [(t, tv', tv'')]
  _ -> return []

-- given the value after typing the branch and the base signature, tells if we can generalise
getReplacements :: Type -> BTree TVar -> (Type, Type) -> Unifying [(Type, Type)]
getReplacements base replacement (candidate, actual) = case (replacement, candidate, actual) of
  (BTree (a, b), TApp a' b', TApp a'' b'') -> do
    rep <- getReplacements base a (a', a'')
    rep' <- getReplacements base b (b', b'')
    return (rep ++ rep')
  (_, TCon t c, _) -> return []
  (Leaf t, TVar _, TVar t') -> return [(TVar t, TVar t')] -- | t' `appearsInType` base]
  (Leaf t, TVar _, _) -> return [(TVar t, actual)] 
  (_, TVar _, _) -> return []
  (a, b, c) -> do 
    tell $ "not covered : " ++ pretty a ++ " | " ++ pretty b ++ " | " ++ pretty c ++ "\n"
    return []

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


allEqs :: [Type] -> Bool
allEqs xs = Set.size (Set.fromList xs) <= 1


type Generalization = Map.Map Type TVar
type TwoSided = (Generalization, Generalization)

instance Pretty TwoSided where
  pretty (a, b) = "(" ++ prettyL (Map.toList a)  ++ ", " ++ prettyL (Map.toList b) ++ ")"

applyG :: Replacable a => Generalization -> a -> a
applyG g = replaceAll (Map.toList $ Map.map TVar g)

applyBoth :: TwoSided -> ArrowType -> ArrowType
applyBoth (a, b) (ArrowType a2 b2) = ArrowType (applyG a a2) (applyG b b2)

mergeG :: [Generalization] -> Generalization
mergeG = foldr (flip Map.union) Map.empty

freshRec :: Unifying String
freshRec = do
  s <- get
  modify (+1)
  return ("'" ++ letters !! s)

type BitVec = [Bool]

bAnd :: BitVec -> BitVec -> BitVec
bAnd a b = getZipList $ (||) <$> ZipList a <*> ZipList b

foldne :: (a -> a -> a) -> [a] -> a
foldne f (x:xs) = foldr f x xs

isValid :: Map.Map Int BitVec -> [Int] -> Bool
isValid m = fmap (m Map.!) >>> foldne bAnd >>> and


fac :: Int -> Int
fac n = product [1..n]

splitToNextSize :: Int -> Int -> [a] -> ([a], [a])
splitToNextSize n k l =
  let next = fac n `div` (fac k * fac (n - k)) in splitAt next l

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
