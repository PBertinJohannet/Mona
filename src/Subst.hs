{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Subst where

import Type
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow
import Pretty
import Syntax
import RecursionSchemes

type Subst = Map.Map TVar Type
type TExpr = Cofree ExprF (Location, Subst, Qual Type); -- expression with information about performed substition (after type inference)

instance Pretty (Location, Subst, Qual Type) where
  pretty (l, s, t) = pretty t ++ " at " ++ pretty l

nullSubst :: Subst
nullSubst = Map.empty

instance Pretty Subst where
  pretty = Map.toList >>> fmap pretty' >>> unwords
    where pretty' (s, a) = pretty s ++ " is " ++ pretty a ++ "\n"

class Substituable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TVar

instance Substituable Type where
  apply s t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TApp` t2) = apply s t1 `TApp` apply s t2
  apply _ t = t

  ftv TCon{} = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 `TApp` t2) = ftv t1 `Set.union` ftv t2

instance Substituable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t -- apply s' maybe
    where s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substituable a => Substituable [a] where
  apply = fmap . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substituable Class where
  apply s (n, insts) = (n, fmap (apply s) insts)
  ftv (n, insts) = foldr (Set.union . ftv) Set.empty insts

instance Substituable t => Substituable (Qual t) where
  apply s (Qual preds t) = Qual (apply s preds) (apply s t)
  ftv (Qual p t) = Set.union (ftv p) (ftv t)

instance Substituable Pred where
  apply s (IsIn n t) = IsIn n (apply s t)
  ftv (IsIn _ t) =  ftv t

instance Substituable TExpr where
  apply s (In ((loc, sub, tp) :< ex)) = In $ (loc, s `compose` sub, apply s tp) :< fmap (apply s) ex
  ftv _ = Set.empty

instance Substituable ArrowType where
  apply s (ArrowType a b) = ArrowType (apply s a) (apply s b)
  ftv _ = Set.empty

instance Substituable (Type, Type) where 
  apply s (a, b) = (apply s a, apply s b)
  ftv (a, b) = ftv a `Set.union` ftv b

mapRes :: (Type -> Type) -> TExpr -> TExpr
mapRes func = mapAnn
  (\(loc, sub, Qual q tp) -> (loc, sub, Qual (mapPred func <$> q) $ func tp))

cleanup :: Subst -> Subst
cleanup subst = Map.fromList $ filter (notLocal . fst) $ Map.toList subst
  where
    notLocal (TV ('\'':_) _) = False
    notLocal _ = True

compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

withFtv :: Type -> Scheme
withFtv t = Forall (Set.toList $ ftv t) $ Qual [] t