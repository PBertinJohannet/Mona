{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Type where
import Pretty
import Control.Arrow

data TVar = TV{name :: String, kind :: Kind}
  deriving (Show, Eq, Ord)

data Kind = Star | Kfun Kind Kind deriving (Show, Eq, Ord)

data Qual t = Qual{preds :: [Pred], head :: t} deriving (Show, Eq, Ord)

data Pred = IsIn String Type deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon String Kind
  | TApp Type Type
  deriving (Show, Eq, Ord)

type Class = ([String], [Inst])
type Inst  = Qual Pred

-- forall a b c . a -> b
data Scheme = Forall [TVar] (Qual Type)
  deriving (Show, Eq, Ord)

var :: String -> TVar
var s = TV{name = s, kind = Star}

tvar :: String -> Type
tvar s = TVar $ var s

typeInt, typeBool, typeList :: Type
typeInt  = TCon "Int" Star
typeBool = TCon "Bool" Star
typeChar = TCon "Char" Star
typeList = TCon "List" $ Kfun Star Star
tArr = TCon "(->)" $ Kfun Star (Kfun Star Star)

-- used to resolve kinds
typeStar :: Type
typeStar = TCon "Star" Star

mkArr :: Type -> Type -> Type
mkArr a = TApp (TApp tArr a)

mkList :: Type -> Type
mkList = TApp typeList

class HasKind a where
  getKind :: a -> Kind

instance HasKind TVar where
  getKind (TV a k) = k

instance HasKind Type where
  getKind = \case
    TVar v -> kind v
    TCon s k -> k
    TApp a b -> case getKind a of
      Kfun _ k -> k

instance Pretty Class where
  pretty = show

instance Pretty Pred where
  pretty (IsIn i t) = "(" ++ i ++ " " ++ pretty t ++ ")"

instance Pretty t => Pretty (Qual t) where
  pretty = \case
    Qual [] t -> pretty t
    Qual p t -> pretty' p ++ " => " ++ pretty t
      where pretty' = fmap pretty >>> unwords

instance Pretty Scheme where
  pretty (Forall [] tp) = pretty tp
  pretty (Forall t tp) = "forall " ++ unwords (fmap pretty t) ++ " . "++ pretty tp

instance Pretty TVar where
  pretty (TV v _) = v

instance Pretty Type where
  pretty = \case
    TVar v -> pretty v
    TCon s k -> s ++ " : " ++ pretty k
    TApp (TApp (TCon "(->)" _) a) b -> case a of
      TApp (TApp (TCon "(->)" _) _) _ ->"(" ++ pretty a ++ ") -> " ++ pretty b
      a -> pretty a ++ " -> " ++ pretty b
    TApp (TCon "List" _) a -> "[" ++ pretty a ++ "]"
    TApp a b -> "(" ++ pretty a ++ " " ++ pretty b ++ ")"

-- a b c

instance Pretty Kind where
  pretty = \case
    Star -> "*"
    Kfun Star Star -> "* -> *"
    Kfun Star k -> "* -> " ++ pretty k
    Kfun k k' -> "(" ++ pretty k ++ ") -> " ++ pretty k'
