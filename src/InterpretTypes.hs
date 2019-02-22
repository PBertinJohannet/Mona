{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
import Data.List

type Interpret a = ReaderT Env (ExceptT TypeError (Writer String)) a;

data UK = UK{
  tname :: String,
  tvars :: [TVar],
  tcons :: Type,
  tp :: Type,
  kd :: Kind,
  patterns :: [(String, Scheme)],
  consts :: [(String, Scheme)]};

instance Pretty (String, Scheme) where
  pretty (s, t) = s ++ " = " ++ showKind t ++ "\n"

instance Pretty UK where
  pretty (UK tname tvars tcons tp kd pats cst) =
    tname ++ " : " ++ pretty kd
    ++ "\nconstructing : " ++ pretty tcons
    ++ "\n of type  : " ++ pretty tp
    ++ "\n with tvars : " ++ unwords (showKind <$> tvars)
    ++ "\n constructors : \n" ++ pretty cst ++ "\n"
    ++ "\n and patterns : \n" ++ pretty pats ++ "\n"

interpret :: [(String, [String], Expr)] -> Envs -> ExceptT TypeError (Writer String) Envs
interpret ds env = foldM (flip runInterpret) env ds

runInterpret :: (String, [String], Expr) -> Envs -> ExceptT TypeError (Writer String) Envs
runInterpret (s, tvars, e) (Envs d v cenv) = do
  Forall _ (Qual _ t) <- inferExpr cenv d e
  tell $ "infered : " ++ showKind t ++ "\n"
  runReaderT (interpretTop (Envs d v cenv) s tvars e t) d

interpretTop :: Envs -> String -> [String] -> Expr -> Type -> Interpret Envs
interpretTop (Envs dat e cenv) name tvars expr inferedType = do
  (env, expr1) <- flattenArgs name expr inferedType
  calls <- apply expr1
  uk <- local (const env) $ createType name tvars calls
  tell $ "created : \n" ++ pretty uk ++ "\n"
  return $ Envs
    (extend dat (name, Forall (var <$> tvars) $ Qual [] (tp uk)))
    (extends e $ consts uk ++ patterns uk)
    cenv

findTV :: String -> Interpret TVar
findTV s = do
  k <- findKind s
  return $ TV s k

createType :: String -> [String] -> [Expr] -> Interpret UK
createType s tvars constructs = do
  vs <- mapM findKind tvars
  tvs <- mapM findTV tvars
  let kind = foldr Kfun Star vs
  let tc =  foldl TApp (TCon s kind) (tvar <$> tvars)
  let schem = Forall tvs $ Qual [] tc
  (cts, pats) <- unzip <$> mapM (makeCons tc tvs) constructs
  return UK {
      tname = s,
      kd = kind,
      tcons = tc,
      tp = foldr mkArr (TVar $ TV "a" Star) (toKindVar <$> tvs),
      tvars = tvs,
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
nextFresh tvars = let Just a = find (`notElem` tvars) letters in a

nextVar :: [TVar] -> Type
nextVar = fmap (\(TV n _) -> n) >>> nextFresh >>> tvar

nextTV :: [TVar] -> TVar
nextTV = fmap (\(TV n _) -> n) >>> nextFresh >>> var

findKind :: String -> Interpret Kind
findKind s = do
  env <- ask
  let Just (Forall _ (Qual _ tp)) = Env.lookup s env
  return $ extractKind tp

extractKind :: Type -> Kind
extractKind = \case
  TVar (TV a _) -> Star
  TCon s k -> k
  TApp (TApp (TCon "(->)" _) a) b -> Kfun (extractKind a) (extractKind b)
  TApp a b -> Kfun (extractKind a) (extractKind b)

flattenArgs :: String -> Expr -> Type -> Interpret (Env, Expr)
flattenArgs name expr tp = do
  env <- ask
  case (expr, tp) of
    (Lam n e, TApp (TCon "(->)" _) b) ->
      return ( extend env (n, Forall [] $ Qual [] b), e)
    (Lam n e, TApp a b) -> do
      (env1, _) <- flattenArgs name expr a
      (env2, e2) <- flattenArgs name e b
      return (mconcat [env1, env, env2], e2)
    _ -> return (env, expr)

apply :: Expr -> Interpret [Expr]
apply expr = do
  env <- ask
  case expr of
    App (App (Var "|") a) b -> do
      b1 <- apply b
      return $ a:b1
    k -> return [k]


makeCons :: Type -> [TVar] -> Expr -> Interpret ((String, Scheme), (String, Scheme))
makeCons baseT tvars expr = do
  (c:argsTp) <- mapM (toType baseT tvars) $ uncurryCall expr
  return ((pretty c,
    Forall tvars $ Qual [] (simplifyUnit $ foldr mkArr baseT argsTp)),
    ("~" ++ pretty c,
    Forall (ret:tvars) $ Qual [] (simplifyUnit $ toPat argsTp)))
    where --
      toPat argsTp = foldr mkArr retv argsTp `mkArr` (baseT `mkArr` retv)
      retv = nextVar tvars
      ret = nextTV tvars


toType :: Type -> [TVar] -> Expr -> Interpret Type
toType baseT tvars e = do
    env <- ask
    case e of
      App a b -> do
        a1 <- toType baseT tvars a
        b1 <- toType baseT tvars b
        return $ TApp a1 b1
      Var v -> do
        k <- findKind v
        return $ maybe (TCon v k) TVar (find (\(TV n k) -> n == v) tvars)
      Fix e -> do
        e1 <- toType baseT tvars e
        return $ TApp e1 baseT
      Lam n e -> toType baseT tvars e

simplifyUnit :: Type -> Type
simplifyUnit = \case
  TApp (TApp (TCon "(->)" _) (TCon "()" _)) b -> b
  TApp a b -> TApp (simplifyUnit a) b
  e -> e
