{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import HMParser

import Control.Arrow
import System.Environment
import System.IO
import qualified Data.Text.Lazy as L
import Env
import Syntax
import Infer
import Text.Parsec (ParseError, SourcePos)
import Pretty
import Type (Scheme, showKind)
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State
import qualified InterpretTypes as DataDecl
import Typeclass (runAddClasses)
import RecursionSchemes
import Sig
import Run
import Operators
import Dispatch
import qualified Data.Map as Map

(<&>) = flip fmap

main = (getArgs <&> head >>= readFile) >>= run >>= putStrLn

data PassErr
 = TypeError TypeError
 | DispatchError DispatchError;

instance Pretty PassErr where
  pretty = \case
    TypeError t -> "TypeError : " ++ pretty t
    DispatchError d -> "DispatchError : " ++ pretty d

run :: String -> IO String
run = L.pack
  >>> parseModule "exampleHM"
  >>> fmap (passes >>> runExceptT >>> runWriter)
  >>> debug --debug . fmap (

instance Pretty (Scheme, Expr) where
  pretty (s, e) = "check : " ++ pretty s ++ " vs " ++ pretty e ++ "\n"

forgetPos :: [(String, Statement)] -> ExceptT PassErr (Writer String) [(String, StatementF Forgot)]
forgetPos = mapM forget'

forget' :: (String, Statement) -> ExceptT PassErr (Writer String) (String, StatementF Forgot)
forget' (s, st) = do
  tell $ show st
  return (s, fmap forget st)

passes :: [(String, Statement)] -> ExceptT PassErr (Writer String) TAst
passes a = do
  let Program exprs datas classes insts sigs = sepDecls a
  tell $ "sigs : \n" ++ prettyL sigs ++ "\n"
  tell $ "datas show : \n" ++ show datas
  tell $ "exprs : \n" ++ prettyL exprs
  tell $ "datas : \n" ++ prettyDatas datas
  env <- withExceptT TypeError $ DataDecl.interpret datas baseEnvs
  (env, insts) <- withExceptT TypeError $ runAddClasses classes insts env
  tell $ "inst to check : " ++ prettyL insts ++ "\n"
  env <- withExceptT TypeError $ addSigs sigs env
  tell $ "after sigs : " ++ showKind env ++ "\n"
  --tell $ pretty env
  --tell $ pretty exprs
  env <- withExceptT TypeError $ inferTop env exprs
  env <- withExceptT TypeError $ checkInstances env insts
  let (Envs _ _ _ TAst{compiled = comp}) = env
  (TAst texprs _) <- withExceptT DispatchError $ runDispatch env
  tell $ "now run  : \n\n" ++ showKind env ++ "\n\n\n"
  return $ TAst texprs comp

exec :: TAst -> IO String
exec (TAst texprs comp) = do
  res <- runProgram $ createRunEnv allNatives texprs comp
  case res of
    Left err -> return $ pretty err
    Right result -> return "\n"

debug :: Either ParseError (Either PassErr TAst, String) -> IO String
debug = \case
  Left perr -> return $ "ParseError : " ++ show perr
  Right (r, s) -> do
    val <- case r of
      Left terr -> return $ pretty terr
      Right v -> exec v
    return $ "\n" ++ val
