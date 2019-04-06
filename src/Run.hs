{-# LANGUAGE LambdaCase #-}
module Erase where
import RecursionSchemes
import Syntax
import Type
import Env
import Subst
import Pretty
import Control.Arrow
import Control.Monad.Except
import Control.Monad.Reader
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

type Interpret = ReaderT RuntimeEnv (Except RunTimeError)

extendRT :: RuntimeEnv -> (Name, Interpret Value) -> RuntimeEnv
extendRT (RTEnv env) (x, s) = RTEnv $ Map.insert x s env

inEnv :: (Name, Value) -> Interpret a -> Interpret a
inEnv (x, sc) m = do
  let scope e = extendRT e (x, return sc)
  local scope m

runProgram :: Envs -> Except RunTimeError Value
runProgram (Envs d c e TAst {texprs = texps}) =
  let env = interpret <$> texps in
  case Map.lookup "main" env of
    Nothing -> throwError MainNotFound
    Just m -> runReaderT m $ RTEnv env

interpret :: TExpr -> Interpret Value
interpret = cataCF $ (uncurry . uncurry) interpretAlg

interpretAlg :: Location -> Type -> ExprF (Interpret Value) -> Interpret Value
interpretAlg loc tp = \case
    Lam x e -> return $ Func $ \val -> inEnv (x, val) e
    k -> do
      k <- sequence k
      interpretAlg' loc tp k

interpretAlg' :: Location -> Type -> ExprF Value -> Interpret Value
interpretAlg' loc tp = \case
  Lit l -> return $ Int l
  Var x -> do
    (RTEnv env) <- ask
    fromMaybe
      (throwError $ ShouldNotHappen $ "cannot find variable " ++ x)
      (Map.lookup x env)
  App a b ->
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
