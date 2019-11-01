{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DataTypes where
import Pretty
import Syntax
import Env (lookup, KEnv(..), Envs(..), letters)
import Type
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Control.Arrow
import Data.Maybe
import Data.List (intercalate, find)
import RecursionSchemes
import Native
import Subst
import Infer (TypeError, InferState(..), initInfer)
import Run (Value(..), Run, makeRunPat, makeRunCons)

runDataDecls :: [DataDecl] -> Envs -> ExceptT TypeError (Writer String) Envs
runDataDecls ds env = foldM runDataDecl env ds

runDataDecl :: Envs-> DataDecl -> ExceptT TypeError (Writer String) Envs
runDataDecl envs@(Envs d v cenv tast) (name, tvars, schemes) = do
  tell "\nenv now : \n"
  tell $ pretty (dataEnv envs)
  tell "do : \n"
  tell $ prettyL schemes
  let typeExpr = foldr (tvar >>> flip TApp) (tvar name) tvars
  --tell $ "match : \n" ++ pretty typeExpr ++ " with " ++ pretty (snd $ Prelude.head schemes) ++ "\n"
  --(_, res) <- runSolve (ClassEnv Map.empty) ((Union . (typeExpr,) . snd) <$> schemes, [])
  -- tell $ pretty res
  tell "\ntry no 3  : \n"
  --runInferKinds envs (snd <$> schemes)
  res <- runInferKind name tvars d printBase
  return envs

type KindVarEnv = Map.Map String KindVar

toLocalKinEnv :: KEnv -> KindVarEnv
toLocalKinEnv (KindEnv kinds) = kTok <$> kinds
  where
    kTok = \case
      Kfun a b -> KVarFun (kTok a) (kTok b)
      Star -> KVarStar

type InferKind = ReaderT KindVarEnv (StateT InferState (ExceptT TypeError (Writer String)))

runInferKind ::  String -> [String] -> KEnv -> InferKind a -> ExceptT TypeError (Writer String) a
runInferKind name tvars kenv inf = evalStateT (runReaderT (makeBase name tvars inf) $ toLocalKinEnv kenv) initInfer

data KindVar = KVarStar | KVarFun KindVar KindVar | KVar String

instance Pretty KindVar where
  pretty = \case
    KVarStar -> "*"
    KVarFun KVarStar KVarStar -> "* -> *"
    KVarFun (KVar s) (KVar s') -> s ++ " -> " ++ s'
    KVarFun KVarStar k -> "* -> " ++ pretty k
    KVarFun (KVar s) k -> s ++ " -> " ++ pretty k
    KVarFun k k' -> "(" ++ pretty k ++ ") -> " ++ pretty k'

extends :: KindVarEnv -> [(Name, KindVar)] -> KindVarEnv
extends env xs = Map.union (Map.fromList xs) env

fresh :: InferKind KindVar
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ KVar (letters !! count s)

inEnv :: [(Name, KindVar)] -> InferKind a -> InferKind a
inEnv x m = do
  let scope e = foldr Map.delete e (fst <$> x) `extends` x
  local scope m

makeBase :: String -> [String] -> InferKind a -> InferKind a
makeBase name tvars p = do
  locals <- makeBaseEnv name tvars
  inEnv locals p

makeBaseEnv :: String -> [String] -> InferKind [(Name, KindVar)]
makeBaseEnv name [] = return [(name, KVarStar)]
makeBaseEnv name (tvar:tvars) = do
  (KVar a) <- fresh
  next <- makeBaseEnv a tvars
  tell $ "adding : " ++ pretty (name, KVarFun (KVar tvar) (KVar a))
  return $ (name, KVarFun (KVar tvar) (KVar a)):next

printBase :: InferKind String
printBase = do
  env <- ask
  tell $ prettyM env
  return "oker"

  {-
runInferKinds :: Envs -> [Type] -> ExceptT TypeError (Writer String) Envs
runInferKinds envs tps = do
  tps <- mapM (runInferKind envs) tps
  return envs

runInferKind :: Envs -> Type -> ExceptT TypeError (Writer String) Type
runInferKind (Envs d v cenv tast) tp = do
  (tps, cons) <- runInferExc d $ runWriterT $ inferKind tp
  tell "types : "
  tell $ pretty tps ++ "\n"
  tell "constraints : \n"
  tell $ pretty cons
  (_, sub) <- runSolve cenv cons
  tell "now solved : \n"
  tell $ pretty sub ++ "end \n\n"
  return tps

runInferExc :: KEnv -> Infer a -> ExceptT TypeError (Writer String) a
runInferExc env m = case runInfer env m of
  Left e -> throwError e
  Right res -> return res

inferKinds :: [Type] -> InferCons [Type]
inferKinds = mapM inferKind

inferKind :: Type -> InferCons Type
inferKind = \case
  TApp a b -> do
    res <- fresh
    a' <- inferKind a
    b' <- inferKind b
    tell $ union (a', b' `mkArr` res)
    return res
  TCon x k -> do
    env <- ask
    (_, Qual _ t) <- lookupEnv x -- `catchError` \e -> lookupEnv $ pretty env
    return t

  TVar x -> return $ TVar x

makeAppAlg :: String -> [String] -> CofreeF ExprF Location [String]
makeAppAlg s = (Loc ("", 0, 0) :<) . \case
  [] -> Var s
  [v] -> Var v
  (tvar:tvars) -> App [tvar] tvars
interpret :: [(String, [String], Expr)] -> Envs -> ExceptT TypeError (Writer String) Envs
interpret ds env = foldM (flip runInterpret) env ds

runInterpret :: (String, [String], Expr) -> Envs -> ExceptT TypeError (Writer String) Envs
runInterpret (s, tvars, e) (Envs d v cenv tast) = do
  let (toInfer, baseConsts, additionalConsts) = desugar tvars e
  tell $ "base : " ++ prettyL baseConsts ++ "\n"
  tell $ "additionals : " ++ prettyL additionalConsts ++ "\n"
  (Forall _ (Qual _ t), _) <- inferExpr cenv d toInfer
  envs <- runReaderT (interpretTop (Envs d v cenv tast) s tvars toInfer t baseConsts) d
  tell $ "giving env : " ++ pretty envs
  foldM addCustomCons envs (reverse additionalConsts)

interpretTop :: Envs -> String -> [String] -> Expr -> Type -> [Expr] -> Interpret Envs
interpretTop (Envs dat e cenv tast) name tvars expr inferedType calls = do
  (env, expr1) <- flattenArgs name expr inferedType
  uk <- local (const env) $ createType name tvars calls
  tell $ "created : \n" ++ pretty uk ++ "\n"
  tell $ "compiled : \n" ++ unwords (fst <$> addRuns (obj uk)) ++ "\n"
  return $ Envs
    (extend dat (name, Forall (var <$> tvars) $ Qual [] (tp uk)))
    (extends e $ consts uk ++ patterns uk)
    cenv
    (withCompiled tast (addRuns $ obj uk))

addCustomCons :: Envs -> Expr -> ExceptT TypeError (Writer String) Envs
addCustomCons (Envs dat val cls tast) ex = do
  let (name, e) = sepCallee ex
  tell $ "got : " ++ pretty e ++ "\n"
  (tp, texp) <- inferExpr cls val e
  tell $ "infered : " ++ pretty tp ++ "\n"
  let infpat = consToPat tp
  return $ Envs dat (val `extends` [(name, tp), ("~"++name, infpat)]) cls (extendAst tast (name, texp))

consToPat :: Scheme -> Scheme
consToPat(Forall tvars (Qual q tp)) = Forall (ret:tvars) (Qual q tp1)
  where
    tp1 = setReturn tp retv `mkArr` (getReturn tp `mkArr` retv)
    retv = nextVar tvars
    ret = nextTV tvars

findTV :: String -> Interpret TVar
findTV s = do
  k <- findKind s
  return $ TV s k

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

addRuns :: Object -> [(String, Run Value)]
addRuns = fmap makeRunPat &&& fmap makeRunCons >>> uncurry (++)

withTags :: [(a, b)] -> [(Int, a , b)]
withTags = zip [0..] >>> fmap (\(a, (b, c)) -> (fromIntegral a, b, c))

makeObject :: [Expr] -> (String, [Expr])
makeObject (In (_ :< Var name):exprs) = (name, exprs)

createType :: String -> [String] -> [Expr] -> Interpret UK
createType s tvars constructs = do
  vs <- mapM findKind tvars
  tvs <- mapM findTV tvars
  let kind = foldr Kfun Star vs
  let tc =  foldl TApp (TCon s kind) (TVar <$> tvs)
  let schem = Forall tvs $ Qual [] tc
  (cts, pats) <- unzip <$> mapM (makeCons tc tvs) constructs
  return UK {
      tname = s,
      kd = kind,
      tcons = tc,
      tp = foldr mkArr (TVar $ TV "a" Star) (toKindVar <$> tvs),
      tvars = tvs,
      obj = withTags (makeObject . uncurryCall <$> constructs),
      patterns = pats,
      consts = cts
    }

toKindVar :: TVar -> Type
toKindVar (TV v k) = kindToVar k

kindToVar :: Kind -> Type
kindToVar = \case
  Kfun a b -> mkArr (kindToVar a) (kindToVar b)
  Star -> tvar "a"

nextFresh :: [String] -> String
nextFresh tvars = let Just a = find (`notElem` tvars) lettersSim in a

nextVar :: [TVar] -> Type
nextVar = fmap (\(TV n _) -> n) >>> nextFresh >>> tvar

nextTV :: [TVar] -> TVar
nextTV = fmap (\(TV n _) -> n) >>> nextFresh >>> var

findKind :: String -> Interpret Kind
findKind s = do
  env <- ask
  return $ case Env.lookup s env of
    Just (Forall _ (Qual _ t)) -> extractKind t
    Nothing -> Star

flattenArgs :: String -> Expr -> Type -> Interpret (Env, Expr)
flattenArgs name expr tp = do
  env <- ask
  case (extract expr, tp) of
    (Lam n e, TApp (TCon "(->)" _) b) ->
      return ( extend env (n, Forall [] $ Qual [] b), e)
    (Lam n e, TApp a b) -> do
      (env1, _) <- flattenArgs name expr a
      (env2, e2) <- flattenArgs name e b
      return (mconcat [env1, env, env2], e2)
    _ -> return (env, expr)

extractCons :: Expr -> [String]
extractCons = histoCF $ snd >>> \case
  App a b -> case matchApp a b of
    (Just ("|", a, b), _) -> b ++ a
    -- (Just ("+", a, b), _) -> b ++ a
    (_, (a, _)) -> a
  Lam n e -> value e
  Var v -> [v]
  Lit l -> ["uh uh"]
  Fix e -> value e

data ConsDeclF a = Or a | Plus a  deriving (Show, Functor);
type ConsDecl = ConsDeclF Expr;

instance Pretty ConsDecl where
  pretty = \case
    Plus a -> "Plus " ++ pretty a
    Or a -> "Or " ++ pretty a

getBaseConsts :: [ConsDecl] -> ([Expr], [Expr])
getBaseConsts = \case
  [] -> ([], [])
  (a:as) -> let (os, ps) = getBaseConsts as in case a of
    (Plus p) -> (os, p:ps)
    (Or o) -> (o:os, ps)

-- removes the (+) operations
desugar :: [String] -> Expr -> (Expr, [Expr], [Expr])
desugar tvars expr =
  let callw = uncurry (:) $ sepCalls Or expr in
  let (b:bs, additionals) = getBaseConsts callw in
  let e = foldr (appC . appC' (varC "|")) b bs in
  let e1 = foldr lamC e (extractCons e) in
  let toInfer = foldr lamC e1 tvars in
  (toInfer, b:bs, additionals)

-- expr found + remaining expr.
sepCalls :: (Expr -> ConsDecl) -> Expr -> (ConsDecl, [ConsDecl])
sepCalls tp = histo $ \(pos :< e) -> case e of
  App a b -> case matchAppCF a b of
    (Just ("|", a, _), _) -> (Or $ orig b, uncurry (:) a)
    -- (Just ("+", a, _), _) -> (Plus $ orig b, uncurry (:) a)
    (_, ((remain, res), b')) -> ((`appC` orig b) <$> remain, res)
  e -> (tp $ inOrig (pos :< e), [])

makeCons :: Type -> [TVar] -> Expr -> Interpret ((String, Scheme), (String, Scheme))
makeCons baseT tvars expr = do
  tell $ "baseT is : " ++ showKind baseT ++ "\n"
  (c:argsTp) <- mapM (toType baseT tvars) $ uncurryCall expr
  return ((pretty c,
    Forall tvars $ Qual [] (simplifyUnit $ foldr mkArr baseT argsTp)),
    ("~" ++ pretty c,
    Forall (ret:tvars) $ Qual [] (simplifyUnit $ toPat argsTp)))
    where
      toPat argsTp = foldr mkArr retv argsTp `mkArr` (baseT `mkArr` retv)
      retv = nextVar tvars
      ret = nextTV tvars

toType :: Type -> [TVar] -> Expr -> Interpret Type
toType baseT tvars = forget >>> cataM (\case
      App a b -> return $ TApp a b
      Var v -> do
        k <- findKind v
        return $ maybe (TCon v k) TVar (find (\(TV n k) -> n == v) tvars)
      Fix e -> return $ TApp e $ unapply baseT
      Lam n e -> return e)

simplifyUnit :: Type -> Type
simplifyUnit = \case
  TApp (TApp (TCon "(->)" _) (TCon "()" _)) b -> b
  TApp a b -> TApp (simplifyUnit a) b
  e -> e
-}
