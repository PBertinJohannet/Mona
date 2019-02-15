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

-- (((flip) (:) [1]) 2)


(<&>) = flip fmap

main = (getArgs <&> head >>= readFile) <&> run >>= putStrLn

run :: String -> String
run = L.pack
  >>> parseModule "ok"
  >>> fmap (Infer.inferTopLog baseClasses baseEnv >>> runWriterT)
  >>> debug --debug . fmap (

instance Pretty [(String, Expr)] where
  pretty = fmap pretty' >>> unwords
    where pretty' (s, e) = s ++ " : " ++ pretty e ++ "\n"

debug :: Either ParseError (Except Infer.TypeError (Env, String)) -> String
debug = \case
  Left perr -> "ParseError : " ++ show perr
  Right sol -> case runExcept sol of
    Left terr -> "TypeError : " ++ show terr
    Right (env, s) -> s ++ "\n" ++ pretty env
