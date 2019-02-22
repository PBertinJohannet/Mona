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
  consts :: [(String, Scheme)]};

instance Pretty (String, Scheme) where
  pretty (s, Forall _ (Qual _ t)) = s ++ " = " ++ showKind t ++ "\n"

instance Pretty UK where
  pretty (UK tname tvars tcons tp kd cst) =
    tname ++ " : " ++ pretty kd
    ++ "\nconstructing : " ++ pretty tcons
    ++ "\n of type  : " ++ pretty tp
    ++ "\n with tvars : " ++ unwords (showKind <$> tvars)
    ++ "\n and constructors : \n" ++ pretty cst ++ "\n"

interpret :: [(String, [String], Expr)] -> Envs -> ExceptT TypeError (Writer String) Envs
interpret ds env = foldM (flip runInterpret) env ds

runInterpret :: (String, [String], Expr) -> Envs -> ExceptT TypeError (Writer String) Envs
runInterpret (s, tvars, e) (Envs d v cenv) = do
  tell $ "infering : " ++ pretty e ++ "\n"
  Forall _ (Qual _ t) <- inferExpr cenv d e
  tell $ "infered : " ++ pretty t ++ "\n"
  runReaderT (interpretTop (Envs d v cenv) s tvars e t) d

interpretTop :: Envs -> String -> [String] -> Expr -> Type -> Interpret Envs
interpretTop (Envs dat e cenv) name tvars expr inferedType = do
  (env, expr1) <- flattenArgs name expr inferedType
  tell $ "env : \n" ++ pretty env ++"\n"
  calls <- apply expr1
  uk <- local (const env) $ createType name tvars calls
  tell $ "created : " ++ pretty uk ++ "\n"
  return $ Envs
    (extend dat (name, Forall (var <$> tvars) $ Qual [] (tp uk)))
    (extends e $ consts uk)
    cenv

findTV :: String -> Interpret TVar
findTV s = do
  k <- findKind s
  return $ TV s k

createType :: String -> [String] -> [Expr] -> Interpret UK
createType s tvars constructs = do
  vs <- mapM findKind tvars
  tvs <- mapM findTV tvars
  let tc =  foldl TApp (tvar s) (tvar <$> tvars)
  let schem = Forall tvs $ Qual [] tc
  cts <- mapM (makeCons tc tvs) constructs
  tell $ "constructs exprs : " ++ pretty constructs ++ "\n"
  return UK {
      tname = s,
      kd = foldr Kfun Star vs,
      tcons = tc,
      tp = foldr mkArr (TVar $ TV "a" Star) (toKindVar <$> tvs),
      tvars = tvs,
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


makeCons :: Type -> [TVar] -> Expr -> Interpret (String, Scheme)
makeCons baseT tvars expr = do
  (c:argsTp) <- mapM (toType baseT tvars) $ uncurryCall expr
  return (pretty c,
    Forall tvars $ Qual [] (simplifyUnit $ foldr mkArr baseT argsTp))

toType :: Type -> [TVar] -> Expr -> Interpret Type
toType baseT tvars e = do
    env <- ask
    case e of
      App a b -> do
        a1 <- toType baseT tvars a
        b1 <- toType baseT tvars b
        return $ TApp a1 b1
      Var v -> return $ maybe (TCon v Star) TVar (find (\(TV n k) -> n == v) tvars)
      Fix e -> do
        e1 <- toType baseT tvars e
        return $ TApp e1 baseT
      Lam n e -> toType baseT tvars e

simplifyUnit :: Type -> Type
simplifyUnit = \case
  TApp (TApp (TCon "(->)" _) (TCon "()" _)) b -> b
  TApp a b -> TApp (simplifyUnit a) b
  e -> e



{-
  = Var v
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit
  | Fix Expr
-}
