{-# LANGUAGE LambdaCase #-}
module Sig where

import Syntax
import Type
import Env
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Infer
import Data.List (find)
import qualified Env (lookup)

type AddSig a = ExceptT TypeError (Writer String) a

addSigs :: [(Location, String, Scheme)] -> Envs -> AddSig Envs
addSigs [] env = return env
addSigs ((loc, name, scheme):ss) env = do
  Envs d v c t <- addSigs ss env
  withCons <- replaceConsTypes [] d scheme `withErrorLoc` loc
  return $ Envs d (v `extend` (name, withCons)) c t

class ReplaceCons a where
  replaceConsTypes :: [TVar] -> KEnv -> a -> AddSig a

instance ReplaceCons Scheme where
  replaceConsTypes _ env (Forall fall (Qual q tp)) = do
    withCons <- replaceConsTypes fall env tp
    return $ Forall fall $ Qual q withCons

instance ReplaceCons Type where
  replaceConsTypes tvars env = \case
    TApp a b -> do
      a1 <- replaceConsTypes tvars env a
      b1 <- replaceConsTypes tvars env b
      return $ TApp a1 b1
    TVar t@(TV name _) -> case find (== t) tvars of
      Just tv -> return $ TVar tv
      Nothing -> case Env.lookup name env of
        Just k -> return $ TCon name k
        Nothing -> throwErrorV $ UnboundVariableInType name
    t -> return t
