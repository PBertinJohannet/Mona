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
import Text.Parsec (ParseError)
import Pretty
import Type (Scheme, showKind)
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State
import InterpretTypes (interpret)
import Typeclass (runAddClasses)
import Sig
-- (((flip) (:) [1]) 2)

(<&>) = flip fmap

main = (getArgs <&> head >>= readFile) <&> run >>= putStrLn

data PassErr = TypeError TypeError | DeclErr DeclErr;

instance Pretty PassErr where
  pretty = \case
    TypeError t -> "TypeError : " ++ pretty t
    DeclErr d -> "DeclarationError : " ++ pretty d

run :: String -> String
run = L.pack
  >>> parseModule "exampleHM"
  >>> fmap (passes >>> runExceptT >>> runWriter)
  >>> debug --debug . fmap (

instance Pretty (Scheme, Expr) where
  pretty (s, e) = "check : " ++ pretty s ++ " vs " ++ pretty e ++ "\n"

passes :: [(String, Statement)] -> ExceptT PassErr (Writer String) Envs
passes l = do
  let Program exprs datas classes insts sigs = sepDecls l
  tell $ "sigs : \n" ++ pretty sigs ++ "\n"
  tell $ "datas : \n" ++ pretty datas
  tell $ "exprs : \n" ++ pretty exprs
  env <- withExceptT TypeError $ interpret datas baseEnvs
  (env, insts) <- withExceptT TypeError $ runAddClasses classes insts env
  tell $ "inst to check : " ++ pretty insts ++ "\n"
  env <- withExceptT TypeError $ addSigs sigs env
  tell $ "after sigs : " ++ showKind env ++ "\n"
  --tell $ pretty env
  --tell $ pretty exprs
  env <- withExceptT TypeError $ inferTop env exprs
  tell $ "after infer : " ++ showKind env ++ "\n"
  withExceptT TypeError $ checkInstances env insts
  return env

debug :: Either ParseError (Either PassErr Envs, String) -> String
debug = \case
  Left perr -> "ParseError : " ++ show perr
  Right (r, s) -> s ++ "\n" ++ case r of
    Left terr -> "TypeError : " ++ pretty terr
    Right env -> pretty env
