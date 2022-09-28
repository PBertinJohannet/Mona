{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Mona (run, compile) where
import Parser
import Control.Arrow
import System.IO
import qualified Data.Text.Lazy as L
import Env
import Syntax
import qualified Infer
import Text.Parsec (ParseError, SourcePos)
import Pretty
import Type (Scheme, showKind)
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State
import qualified DataTypes as DataDecl
import Typeclass (runAddClasses)
import RecursionSchemes
import Sig
import qualified Run
import Dispatch
import Native
import qualified Data.Map as Map
import Data.List

data PassErr
 = TypeError Infer.TypeError
 | DataDeclError DataDecl.DataDeclError
 | DispatchError DispatchError;

instance Pretty PassErr where
  pretty = \case
    DataDeclError d -> "DataDeclError : " ++ pretty d
    TypeError t -> "TypeError : " ++ pretty t
    DispatchError d -> "DispatchError : " ++ pretty d

run :: String -> IO String
run = L.pack
  >>> parseModule "fileName"
  >>> fmap (passes >>> runExceptT >>> runWriter)
  >>> debug --debug . fmap (

compile :: String -> Either String (Envs)
compile = id L.pack
  >>> parseModule "fileName"
  >>> fmap (passes >>> runExceptT >>> runWriter)
  >>> recap 

instance Pretty (Scheme, Expr) where
  pretty (s, e) = "check : " ++ pretty s ++ " vs " ++ pretty e ++ "\n"
{-
forgetPos :: [(String, Statement)] -> ExceptT PassErr (Writer String) [(String, StatementF Forgot)]
forgetPos = mapM forget'

forget' :: (String, Statement) -> ExceptT PassErr (Writer String) (String, StatementF Forgot)
forget' (s, st) = do
  tell $ show st
  return (s, fmap forget st)
-}
passes :: [(String, Statement)] -> ExceptT PassErr (Writer String) Envs
passes a = do
  tell $ mconcat $ intersperse "\n" (show <$> a)
  let Program exprs datas classes insts sigs = sepDecls a
  tell $ "sigs : \n" ++ prettyL sigs ++ "\n"
  tell $ "datas show : \n" ++ show datas
  tell $ "exprs : \n" ++ prettyL exprs
  tell $ "datas : \n" ++ prettyDatas datas
  env <- withExceptT DataDeclError $ DataDecl.runDataDecls datas baseEnvs
  tell $ "inst to check before : " ++ show insts ++ "\n"
  (env, insts) <- withExceptT TypeError $ runAddClasses classes insts env
  tell $ "before sigs : " ++ pretty (varEnv env) ++ "\n"
  tell $ "sigs to add : " ++ prettyL sigs ++ "\n"
  env <- withExceptT TypeError $ addSigs sigs env
  tell $ "after sigs : " ++ pretty (varEnv env) ++ "\n"
  tell $ pretty env
  --tell $ pretty exprs
  --tell $ "infering : " ++ mconcat (intersperse "\n" (pretty <$> exprs))
  env <- withExceptT TypeError $ Infer.inferTop env exprs
  env <- withExceptT TypeError $ Infer.checkInstances env insts
  let (Envs dataEnv var classEnv TAst{compiled = comp}) = env
  (TAst texprs _) <- withExceptT DispatchError $ runDispatch env
  tell $ "now run  : \n\n" ++ showKind env ++ "\n\n\n"
  return $ Envs dataEnv var classEnv (TAst texprs comp)

exec :: TAst -> IO String
exec (TAst texprs comp) = do
  res <- Run.runProgram $ Run.createRunEnv allNatives texprs comp
  case res of
    Left err -> return $ pretty err
    Right result -> return ""

debug :: Either ParseError (Either PassErr Envs, String) -> IO String
debug = \case
  Left perr -> return $ "ParseError : " ++ show perr
  Right (r, s) -> do
    putStrLn s
    case r of
      Left terr -> return $ pretty terr
      Right (Envs _ _ _ ast) -> exec ast

recap :: Either ParseError (Either PassErr Envs, String) -> Either String Envs
recap = \case
  Left perr -> Left $ "ParseError : " ++ show perr
  Right (r, s) -> left pretty r
