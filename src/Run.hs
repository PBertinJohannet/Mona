{-# LANGUAGE LambdaCase #-}
module Run where
import RecursionSchemes
import Syntax
import Type
import Pretty
import Control.Arrow
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe
import Data.List
import qualified Data.Map as Map

data RunTimeError
 = ShouldNotHappen String
 | DivByZero Location
 | CaseMissing Location
 | ForbidenPattern String
 | MainNotFound

instance Pretty RunTimeError where
  pretty = \case
    ShouldNotHappen s -> "Should not happen : " ++ s ++ "\n"
    DivByZero d -> "Division by zero at : " ++ pretty d ++ "\n"
    CaseMissing d -> "Incomplete case patterns at : " ++ pretty d ++ "\n"
    MainNotFound -> "Main not found\n"

data Value
  = Int Int
  | Char Char
  | Variant Int Value
  | Prod [Value]
  | Func (Value -> Run Value)
  | PatFail

instance Pretty Value where
  pretty = \case
    Int i -> show i
    Char c -> show c
    Variant i b -> "case " ++ show i ++ " of " ++ pretty b
    Prod ps -> inParen $ intercalate "," (pretty <$> ps)
    Func f -> "<function>"

newtype RuntimeEnv = RTEnv (Map.Map String (Run Value))

instance Pretty RuntimeEnv where
  pretty (RTEnv t) = mconcat . fmap showAssoc . Map.toList $ t
    where showAssoc (n, s) = n ++ ", "

type Run = ReaderT RuntimeEnv (ExceptT RunTimeError (Writer String))

extendRT :: RuntimeEnv -> (Name, Run Value) -> RuntimeEnv
extendRT (RTEnv env) (x, s) = RTEnv $ Map.insert x s env

removeRT :: RuntimeEnv -> Name -> RuntimeEnv
removeRT (RTEnv env) n = RTEnv $ Map.delete n env

inEnv :: (Name, Value) -> Run a -> Run a
inEnv (x, sc) m = do
  let scope e = removeRT e x `extendRT` (x, return sc)
  local scope m

createRunEnv :: Map.Map String Value -> Map.Map String TExpr -> Map.Map String (Run Value)
createRunEnv nat src = Map.union (return <$> nat) (interpret <$> src)

runProgram :: Map.Map String (Run Value) -> ExceptT RunTimeError (Writer String) Value
runProgram env = case Map.lookup "main" env of
    Nothing -> throwError MainNotFound
    Just m -> runReaderT m $ RTEnv env

interpret :: TExpr -> Run Value
interpret = cataCF $ (uncurry . uncurry) interpretAlg

interpretAlg :: Location -> Type -> ExprF (Run Value) -> Run Value
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

interpretAlg' :: Location -> Type -> ExprF Value -> Run Value
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

changeCase :: Value -> Value -> Value -> Run Value
changeCase src result other = case (result, other) of
  (PatFail, Func f) -> f src
  (ok, Func _) -> return ok
  (_, a) -> throwError $ ShouldNotHappen $ "applying a non function pattern " ++ pretty a


runIf :: Value
runIf = Func (\c -> return $ Func (\t -> return $ Func (\f -> case c of
  Int i -> return $ if i == 0 then f else t
  a ->  throwError $ ShouldNotHappen $ "Non boolean in if " ++ pretty a)))

mkRun :: (Int -> Int -> Int) -> Value
mkRun f = Func (\a -> return $ Func (\b -> case (a, b) of
  (Int a, Int b) -> return $ Int $ f a b
  (a, b) -> throwError $ ShouldNotHappen $ "Non integer in native " ++ pretty a))

runCompose :: Value
runCompose = Func (\a -> return $ Func (\b -> case (a, b) of
  (Func a, Func b) -> return $ Func (a <=< b)
  (a, b) -> throwError $ ShouldNotHappen $ "Non func in native " ++ pretty a))

makeRunCons :: (Int, String, [Expr]) -> (String, Run Value)
makeRunCons (tag, name, exprs) = (name, do
  prods <- mapM (cataCFM_ consAlg) exprs
  return $ Variant tag $ Prod prods)

consAlg :: ExprF Value -> Run Value
consAlg = \case
  Var x -> do
    (RTEnv env) <- ask
    fromMaybe
      (throwError $ ShouldNotHappen $ "cannot find constructor " ++ x)
      (Map.lookup x env)
  App a b ->
    case a of
      Func f -> f b
      e -> throwError $ ShouldNotHappen $ "applying a non function " ++ pretty a ++ " to an arg"
  Fix t1 -> case t1 of
    Func f -> f t1
    e -> throwError $ ShouldNotHappen $ "applying fix to a non function " ++ pretty t1
  Case src pats -> throwError $ ForbidenPattern "Case"
  Lam src pats -> throwError $ ForbidenPattern "Lambda"
  Lit l -> throwError $ ForbidenPattern "Int"


makeRunPat :: (Int, String, [Expr]) -> (String, Run Value)
makeRunPat (tag, name, exprs) = (name, return $ Func $ return . Func . runPat exprs tag)

runPat :: [Expr] -> Int -> Value -> Value -> Run Value
runPat exps tag func (Variant tag' _) | tag /= tag' = return PatFail
runPat exps _ func (Variant _ (Prod vals)) = do
  let tuples = zip exps vals
  
  return $ Int 1
