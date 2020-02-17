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
import Subst

data RunTimeError
 = ShouldNotHappen String
 | DivByZero Location
 | CaseMissing Location
 | ForbidenPattern String
 | MainNotFound

instance Pretty RunTimeError where
  pretty = \case
    ShouldNotHappen s -> "Should never happen : " ++ s ++ "\n"
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

debug :: Value -> Run String
debug = \case
  Func f -> pretty <$> f (Int 1)
  e -> return $ pretty e

newtype RuntimeEnv = RTEnv (Map.Map String (Run Value))

instance Pretty RuntimeEnv where
  pretty (RTEnv t) = mconcat . fmap showAssoc . Map.toList $ t
    where showAssoc (n, s) = n ++ ", "

type Run = ReaderT RuntimeEnv (ExceptT RunTimeError IO)

extendRT :: RuntimeEnv -> (Name, Run Value) -> RuntimeEnv
extendRT (RTEnv env) (x, s) = RTEnv $ Map.insert x s env

removeRT :: RuntimeEnv -> Name -> RuntimeEnv
removeRT (RTEnv env) n = RTEnv $ Map.delete n env

inEnv :: (Name, Value) -> Run a -> Run a
inEnv (x, sc) m = do
  let scope e = removeRT e x `extendRT` (x, return sc)
  local scope m

type MapS a = Map.Map String a

createRunEnv :: MapS Value -> MapS TExpr -> MapS (Run Value) -> MapS (Run Value)
createRunEnv nat src = Map.union $ Map.union (return <$> nat) (interpret <$> src)

runProgram :: Map.Map String (Run Value) -> IO (Either RunTimeError Value)
runProgram env = case Map.lookup "main" env of
    Nothing -> return $ Left MainNotFound
    Just m -> runExceptT $ runReaderT m $ RTEnv env

interpret :: TExpr -> Run Value
interpret = ($ ()) <<< cataCFLazy (uncurry interpretAlg)

interpretAlg :: (Location, TSubst, Qual Type Type) -> ExprF (() -> Run Value) -> Run Value
interpretAlg b = \case
  Case src pats -> do
    src <- src ()
    foldM (changeCase src) PatFail (getExp <$> pats)
  e -> interpretAlg' b (($ ()) <$> e)

interpretAlg' :: (Location, TSubst, Qual Type Type) -> ExprF (Run Value) -> Run Value
interpretAlg' (loc, sub, Qual p tp) e =
  case e of
    Lam (PatternT x e) -> do
      thisEnv <- ask
      return $ Func $ \val -> local (const thisEnv) (inEnv ("x", val) e) -- adding is adding to nothing, this is going to be an error sooon.
    k -> do
      k <- sequence k
      interpretAlg'' loc sub tp k

interpretAlg'' :: Location -> TSubst -> Type -> ExprF Value -> Run Value
interpretAlg'' loc sub tp = \case
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

changeCase :: Value -> Value -> (() -> Run Value) -> Run Value
changeCase src result other = case result of
  PatFail -> do
    other <- other ()
    case other of
      Func f -> f src
      a -> throwError $ ShouldNotHappen $ "applying a non function pattern " ++ pretty a
  ok -> return ok

runCompose :: Value
runCompose = Func (\a -> return $ Func (\b -> case (a, b) of
  (Func a, Func b) -> return $ Func (a <=< b)
  (a, b) -> throwError $ ShouldNotHappen $ "Non func in native " ++ pretty a))

makeRunCons :: (Int, Int, String) -> (String, Run Value)
makeRunCons (nbVals, tag, name) = (name, construct tag nbVals [])

construct :: Int -> Int -> [Value] -> Run Value
construct tag 0 prods = return $ Variant tag $ Prod prods
construct tag n prods = return $ Func (\val -> construct tag (n-1) (prods ++ [val]))

applyVal :: Value -> Value -> Run Value
applyVal a b = case a of
  Func f -> f b
  e -> throwError $ ShouldNotHappen $ "applying a non function " ++ pretty a ++ " to an arg " ++ pretty e ++ "\n"

makeRunPat :: (Int, String) -> (String, Run Value)
makeRunPat (tag, name) = ("~" ++ name, return $ Func $ return . Func . runPat name tag)

runPat :: String -> Int -> Value -> Value -> Run Value
runPat _ tag func (Variant tag' _) | tag /= tag' = return PatFail
runPat _ _ func (Variant _ (Prod vals)) = foldM applyVal func vals
runPat s _ _ v = do
  val <- debug v
  throwError $ ShouldNotHappen $ "Pattern called on non Object" ++ val ++ " at : " ++ s

unitVariant :: Int -> Value
unitVariant i = Variant i $ Prod []

-- base run functions.

runPrintInt :: Value
runPrintInt = Func (\case
  Int c -> do
    liftIO $ putStr $ show c
    return $ unitVariant 0
  e -> throwError $ ShouldNotHappen $ "excpecting an int to PrintInt" ++ pretty e)

runEndl :: Value
runEndl = Char '\n'

runPrintChar :: Value
runPrintChar = Func (\case
  Char c -> do
    liftIO $ putChar c
    return $ unitVariant 0
  _ -> throwError $ ShouldNotHappen "excpecting a char to PrintChar")

runEquals :: Value
runEquals = Func (\a -> return $ Func (\b -> case (a, b) of
  (Int a, Int b) -> return $ if a == b then unitVariant 1 else unitVariant 0
  (a, b) -> throwError $ ShouldNotHappen $ "Non integer in equals " ++ pretty a))

mkRun :: String -> (Int -> Int -> Int) -> Value
mkRun s f = Func (\a -> return $ Func (\b -> case (a, b) of
  (Int a, Int b) -> return $ Int $ f a b
  (a, b) -> throwError $ ShouldNotHappen $ "Non integer in native " ++ pretty a))
