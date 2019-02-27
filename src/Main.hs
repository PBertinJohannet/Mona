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
import InterpretTypes
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

instance Pretty (String, [String], Expr) where
  pretty (name, tvars, ex) = "type " ++ name ++ " " ++ unwords tvars ++ " = " ++ pretty ex ++ "\n"

passes :: [(String, Statement)] -> ExceptT PassErr (Writer String) Envs
passes l = do
  let Program exprs datas classes sigs = sepDecls l
  tell $ "sigs : \n" ++ pretty sigs ++ "\n"
  tell $ "datas : \n" ++ pretty datas
  tell $ "exprs : \n" ++ pretty exprs
  env0 <- withExceptT TypeError $ interpret datas baseEnvs
  env1 <- withExceptT TypeError $ addSigs sigs env0
  tell $ "after sigs : " ++ showKind env1 ++ "\n"
  --tell $ pretty env
  --tell $ pretty exprs
  withExceptT TypeError $ inferTop env1 exprs

instance Pretty (String, Expr) where
  pretty (s, e) = s ++ " : " ++ pretty e ++ "\n"

debug :: Either ParseError (Either PassErr Envs, String) -> String
debug = \case
  Left perr -> "ParseError : " ++ show perr
  Right (r, s) -> s ++ "\n" ++ case r of
    Left terr -> "TypeError : " ++ pretty terr
    Right env -> pretty env
