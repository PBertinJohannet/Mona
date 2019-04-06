{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module InterpretTypes where
import Pretty
import Syntax
import Env
import qualified Env (lookup)
import Type
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map as Map
import Control.Arrow
import Data.Maybe
import Infer
import Data.List (intercalate, find)
import RecursionSchemes
import Run (Value(..), Run, makeRunPat, makeRunCons)

type Interpret a = ReaderT Env (ExceptT TypeError (Writer String)) a;

type Object = [(Int, String, [Expr])]

instance Pretty Object where
  pretty = fmap pretty' >>> unwords
    where pretty' (v, n, e) = inParen $ "Variant " ++ show v ++ inParen n ++ " " ++ prettyL e

data UK = UK{
  tname :: String,
  tvars :: [TVar],
  tcons :: Type,
  tp :: Type,
  kd :: Kind,
  obj :: Object,
  patterns :: [(String, Scheme)],
  consts :: [(String, Scheme)]};

instance ShowKind (String, Scheme) where
  showKind (s, t) = s ++ " = " ++ showKind t ++ "\n"

instance Pretty UK where
  pretty (UK tname tvars tcons tp kd obj pats cst) =
    tname ++ " : " ++ pretty kd
    ++ "\nconstructing : " ++ pretty tcons
    ++ "\n of type  : " ++ pretty tp
    ++ "\n with tvars : " ++ unwords (showKind <$> tvars)
    ++ "\n constructors : \n" ++ showKind cst ++ "\n"
    ++ "\n object : \n" ++ pretty obj ++ "\n"
    ++ "\n and patterns : \n" ++ showKind pats ++ "\n"

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
