{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Subst where

import Type
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow
import Pretty
import Syntax
import RecursionSchemes



type Subst a = Map.Map TVar a
type TSubst = Subst Type
type VSubst = Subst Variational
type PExpr a = Cofree ExprF (Location, Subst a, Qual a a);
type VExpr = PExpr Variational; -- expression with information about performed substition (after type inference)
type TExpr = PExpr Type; -- expression with information about performed substition (after type inference)

instance Pretty a => Pretty (Location, Subst a, Qual Type Type) where
  pretty (l, s, t) = pretty t ++ " at " ++ pretty l

instance Pretty a => Pretty (Location, Subst a, Qual Variational Variational) where
  pretty (l, s, t) = pretty t ++ " at " ++ pretty l

class Substitute a where
  nullSubst :: Subst a

instance Substitute Type where
  nullSubst = Map.empty

instance Substitute Variational where
  nullSubst = Map.empty

instance Pretty a => Pretty (Subst a) where
  pretty = Map.toList >>> fmap pretty' >>> unwords
    where pretty' (s, a) = pretty s ++ " is " ++ pretty a ++ "\n"

class Parametrized a where
  ftv :: a -> Set.Set TVar

class (Parametrized a) => Substituable a b where
  apply :: Subst b -> a -> a

instance Substituable a a => Substituable (PExpr a) a where
  apply s = mapAnn applyAnn 
    where applyAnn (loc, sub, qual) = (loc, s `compose` sub, apply s qual)

instance Parametrized a => Parametrized (PExpr a) where
  ftv = const Set.empty

instance Substituable Type Type where
  apply s t@(TVar a) = Map.findWithDefault t a s
  apply s (t1 `TApp` t2) = apply s t1 `TApp` apply s t2
  apply _ t = t

instance Parametrized Type where
  ftv TCon{} = Set.empty
  ftv (TVar a) = Set.singleton a
  ftv (t1 `TApp` t2) = ftv t1 `Set.union` ftv t2

instance Substituable Variational Type where
  apply s = \case
    TCon s k -> TCon s k
    Dim n ts -> Dim n (apply s <$> ts)
    TApp t1 t2 -> apply s t1 `VApp` apply s t2

instance Substituable Variational Variational where
  apply s = \case 
    Dim n ts -> Dim n (apply s <$> ts)
    TApp t1 t2 -> apply s t1 `VApp` apply s t2
    TPlus t -> Plain $ apply s' t
      where 
        s' :: Subst Type
        s' = Map.mapMaybe extractPlains s 
        extractPlains = \case
          Plain t -> Just t
          _ -> Nothing

instance Parametrized Variational where
  ftv = \case
    Plain t -> ftv t
    Dim s ts -> foldr Set.union Set.empty (ftv <$> ts)
    VApp t1 t2 -> ftv t1 `Set.union` ftv t2

instance Substituable Scheme Type where
  apply s (Forall as t) = Forall as $ apply s' t -- apply s' maybe
    where s' = foldr Map.delete s as

instance Parametrized Scheme where
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substituable a b => Substituable [a] b where
  apply = fmap . apply
instance Parametrized a => Parametrized [a] where
  ftv = foldr (Set.union . ftv) Set.empty

instance Substituable Class Type where
  apply s (n, insts) = (n, fmap (apply s) insts)

instance Parametrized Class where
  ftv (n, insts) = foldr (Set.union . ftv) Set.empty insts

instance (Substituable t b, Substituable a b) => Substituable (Qual a t) b where
  apply s (Qual preds t) = Qual (apply s preds) (apply s t)
instance (Parametrized a, Parametrized t) => Parametrized (Qual a t) where
  ftv (Qual p t) = Set.union (ftv p) (ftv t)

instance (Substituable a b) => Substituable (Pred a) b where
  apply s (IsIn n t) = IsIn n (apply s t)
instance Parametrized a => Parametrized (Pred a) where
  ftv (IsIn _ t) =  ftv t

instance (Substituable (Pred Type, Location) Type) where
  apply s (t, l) = (apply s t, l)
instance (Parametrized (Pred Type, Location)) where
  ftv (t, l) = ftv t

mapRes :: (Type -> Type) -> TExpr -> TExpr
mapRes func = mapAnn
  (\(loc, sub, Qual q tp) -> (loc, sub, Qual (mapPred func <$> q) $ func tp))

cleanup :: Subst a -> Subst a
cleanup subst = Map.fromList $ filter (notLocal . fst) $ Map.toList subst
  where
    notLocal (TV ('\'':_) _) = False
    notLocal _ = True

compose :: (Substituable a a) => Subst a -> Subst a -> Subst a
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

withFtv :: Type -> Scheme
withFtv t = Forall (Set.toList $ ftv t) $ Qual [] t

dom :: NonEmpty Variational -> Set.Set TVar
dom x = foldr Set.union Set.empty (ftv <$> x)
