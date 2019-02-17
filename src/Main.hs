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
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State

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
  >>> parseModule "ok"
  >>> fmap (passes >>> runExceptT >>> runWriter)
  >>> debug --debug . fmap (

passes :: [(String, Statement)] -> ExceptT PassErr (Writer String) Env
passes l = do
  (exprs, env) <- withExceptT DeclErr $ runTypeDecls baseEnv l
  env1 <- withExceptT TypeError $ inferTop baseClasses env exprs
  return env1
  -- >>> flip runStateT baseEnv
  -- >>> withExceptT DeclErr
--  >>> ((DeclErr +++ id) &&& id)
  -- >>> _ok


instance Pretty (String, Expr) where
  pretty (s, e) = s ++ " : " ++ pretty e ++ "\n"

debug :: Either ParseError (Either PassErr Env, String) -> String
debug = \case
  Left perr -> "ParseError : " ++ show perr
  Right (r, s) -> s ++ "\n" ++ case r of
    Left terr -> "TypeError : " ++ pretty terr
    Right env -> pretty env
