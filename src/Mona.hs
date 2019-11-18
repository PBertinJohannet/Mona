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

compile :: String -> String
compile = L.pack
  >>> parseModule "fileName"
  >>> fmap (passes >>> runExceptT >>> runWriter)
  >>> debugPure

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
passes :: [(String, Statement)] -> ExceptT PassErr (Writer String) TAst
passes a = do
  let Program exprs datas classes insts sigs = sepDecls a
  tell $ "sigs : \n" ++ prettyL sigs ++ "\n"
  tell $ "datas show : \n" ++ show datas
  tell $ "exprs : \n" ++ prettyL exprs
  tell $ "datas : \n" ++ prettyDatas datas
  env <- withExceptT DataDeclError $ DataDecl.runDataDecls datas baseEnvs
  (env, insts) <- withExceptT TypeError $ runAddClasses classes insts env
  tell $ "inst to check : " ++ prettyL insts ++ "\n"
  env <- withExceptT TypeError $ addSigs sigs env
  tell $ "after sigs : " ++ showKind env ++ "\n"
  --tell $ pretty env
  --tell $ pretty exprs
  env <- withExceptT TypeError $ Infer.inferTop env exprs
  env <- withExceptT TypeError $ Infer.checkInstances env insts
  let (Envs _ _ _ TAst{compiled = comp}) = env
  (TAst texprs _) <- withExceptT DispatchError $ runDispatch env
  tell $ "now run  : \n\n" ++ showKind env ++ "\n\n\n"
  return $ TAst texprs comp

exec :: TAst -> IO String
exec (TAst texprs comp) = do
  res <- Run.runProgram $ Run.createRunEnv allNatives texprs comp
  case res of
    Left err -> return $ pretty err
    Right result -> return ""

debug :: Either ParseError (Either PassErr TAst, String) -> IO String
debug = \case
  Left perr -> return $ "ParseError : " ++ show perr
  Right (r, s) ->
    case r of
      Left terr -> return $ pretty terr
      Right v -> exec v

debugPure :: Either ParseError (Either PassErr TAst, String) -> String
debugPure = \case
  Left perr -> "ParseError : " ++ show perr
  Right (r, s) ->
    case r of
      Left terr -> pretty terr
      Right v -> "compiled successfully"
