module Typeclass where

import Type
import Env
import Control.Monad.Except
import Control.Monad.Writer
import Infer
import Data.List (find)
import Sig
import Control.Arrow
import qualified Env (lookup)

type AddClass a = ExceptT TypeError (Writer String) a

addClasses :: [(String, String, [(String, Scheme)])] -> Envs -> AddSig Envs
addClasses [] env = return env
addClasses ((name, v, sigs):ss) env = do
  env1 <- addClasses ss env
  addSigs (second (withPred v (IsIn name $ tvar v)) <$> sigs) env1
