{-# LANGUAGE LambdaCase #-}

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

type Interpret a = ReaderT Env (ExceptT () (Writer String)) a;

interpret :: [(String, [String], Expr)] -> Envs -> ExceptT () (Writer String) Env
interpret decls (Envs env _ _) = do
  mapM_ (`runInterpret` env) decls
  return env

runInterpret :: (String, [String], Expr) -> Env -> ExceptT () (Writer String) Env
runInterpret (s, tvars, e) = runReaderT (interpretTop s tvars e)

inEnv :: (String, Scheme) -> Interpret a -> Interpret a
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

interpretTop :: String -> [String] -> Expr -> Interpret Env
interpretTop name tvars expr = do
  env <- ask
  let Just (Forall _ (Qual _ tp)) = Env.lookup name env
  (env, expr1) <- flattenArgs name expr tp
  call <- apply expr1
  newType <- local (const env) $ createType name tvars
  tell $ "made type : " ++ pretty newType ++ "\n"
  return env

createType :: String -> [String] -> Interpret Type
createType s tvars = do
  vs <- mapM findKind tvars
  return $ TCon s $ foldr Kfun Star vs

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

data Call = Call String [String] deriving Show

mergeCall :: Call -> Call -> Call
mergeCall (Call s k) (Call s' k') = Call s $ k ++ [s'] ++ k'

apply :: Expr -> Interpret Call
apply expr = do
  env <- ask
  case expr of
    Var v -> return $ Call v []
    App a b -> do
      a1 <- apply a
      b1 <- apply b
      return $ mergeCall a1 b1
    k -> do
      tell $ show k
      throwError ()

{-
  = Var v
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit
  | Fix Expr
-}
