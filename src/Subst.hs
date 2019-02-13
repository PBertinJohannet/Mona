{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Subst where

import Type
import Env
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow
import Pretty

type Subst = Map.Map TVar Type

nullSubst :: Subst
nullSubst = Map.empty

instance Pretty Subst where
  pretty = Map.toList >>> fmap pretty' >>> unwords
    where pretty' (s, a) = pretty s ++ " is " ++ pretty a ++ "\n"

class Substituable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TVar

instance Substituable Type where
  apply _ (TCon a) = TCon a
  apply s t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

  ftv TCon{} = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance Substituable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t -- apply s' maybe
    where s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substituable a => Substituable [a] where
  apply = fmap . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substituable Env where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env


compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1
