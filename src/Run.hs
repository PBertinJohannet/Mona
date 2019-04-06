{-# LANGUAGE LambdaCase #-}
module Run where
import RecursionSchemes
import Syntax
import Type
import Env
import Subst
import Pretty
import Control.Arrow
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

data RunTimeError
 = ShouldNotHappen String
 | DivByZero Location
 | CaseMissing Location
 | MainNotFound

instance Pretty RunTimeError where
  pretty = \case
    ShouldNotHappen s -> "Should not happen : " ++ s ++ "\n"
    DivByZero d -> "Division by zero at : " ++ pretty d ++ "\n"
    CaseMissing d -> "Incomplete case patterns at : " ++ pretty d ++ "\n"
    MainNotFound -> "Main not found\n"

data Value
  = Int Integer
  | Char Char
  | Variant Int Value
  | Prod Value Value
  | Func (Value -> Interpret Value)
  | PatFail

instance Pretty Value where
  pretty = \case
    Int i -> show i
    Char c -> show c
    Variant i b -> "case " ++ show i ++ " of " ++ pretty b
    Prod a b -> pretty a ++ ",  " ++ pretty b
    Func f -> "<function>"

newtype RuntimeEnv = RTEnv (Map.Map String (Interpret Value))

instance Pretty RuntimeEnv where
  pretty (RTEnv t) = mconcat . fmap showAssoc . Map.toList $ t
    where showAssoc (n, s) = n ++ ", "

type Interpret = ReaderT RuntimeEnv (ExceptT RunTimeError (Writer String))

extendRT :: RuntimeEnv -> (Name, Interpret Value) -> RuntimeEnv
extendRT (RTEnv env) (x, s) = RTEnv $ Map.insert x s env

removeRT :: RuntimeEnv -> Name -> RuntimeEnv
removeRT (RTEnv env) n = RTEnv $ Map.delete n env

inEnv :: (Name, Value) -> Interpret a -> Interpret a
inEnv (x, sc) m = do
  let scope e = removeRT e x `extendRT` (x, return sc)
  local scope m

runProgram :: Envs -> ExceptT RunTimeError (Writer String) Value
runProgram (Envs d c e TAst {texprs = texps}) =
  let env = interpret <$> texps in
  case Map.lookup "main" env of
    Nothing -> throwError MainNotFound
    Just m -> runReaderT m $ RTEnv env

interpret :: TExpr -> Interpret Value
interpret = cataCF $ (uncurry . uncurry) interpretAlg

interpretAlg :: Location -> Type -> ExprF (Interpret Value) -> Interpret Value
interpretAlg loc tp e = do
  (RTEnv env) <- ask
  tell $ "at : " ++ pretty loc ++ "\n"
  tell $ "expr :" ++ prettyShape e ++ "\n"
  tell $ "env :" ++ pretty (RTEnv env) ++ "\n"
  res <- case e of
    Lam x e -> do
      thisEnv <- ask
      return $ Func $ \val -> local (const thisEnv) (inEnv (x, val) e) -- adding is adding to nothing.
    k -> do
      k <- sequence k
      interpretAlg' loc tp k
  tell $ "returning : " ++ pretty res ++ "\n\n"
  return res

interpretAlg' :: Location -> Type -> ExprF Value -> Interpret Value
interpretAlg' loc tp = \case
  Lit l -> return $ Int l
  Var x -> do
    (RTEnv env) <- ask
    --tell $ "env :" ++ pretty (RTEnv env)
    fromMaybe
      (throwError $ ShouldNotHappen $ "cannot find variable " ++ x)
      (Map.lookup x env)
  App a b -> do
    tell $ "computed b : " ++ pretty b ++ "before calling the function \n"
    tell $ "computed a : " ++ pretty a ++ "before calling the function \n"
    case a of
      Func f -> f b
      e -> throwError $ ShouldNotHappen $ "applying a non function " ++ pretty a ++ " to an arg"
  Fix t1 -> case t1 of
    Func f -> f t1
    e -> throwError $ ShouldNotHappen $ "applying fix to a non function " ++ pretty t1
  Case src pats -> foldM (changeCase src) PatFail pats

changeCase :: Value -> Value -> Value -> Interpret Value
changeCase src result other = case (result, other) of
  (PatFail, Func f) -> f src
  (ok, Func _) -> return ok
  (_, a) -> throwError $ ShouldNotHappen $ "applying a non function pattern " ++ pretty a
