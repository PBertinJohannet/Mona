{-# LANGUAGE LambdaCase #-}

module Type where
import Pretty

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon String
  | TArr Type Type
  deriving (Show, Eq, Ord)

-- forall a b c . a -> b
data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

typeInt, typeBool :: Type
typeInt  = TCon "Int"
typeBool = TCon "Bool"

instance Pretty Scheme where
  pretty (Forall t tp) = "forall " ++ unwords (fmap pretty t) ++ " . "++ pretty tp

instance Pretty TVar where
  pretty (TV v) = v

instance Pretty Type where
  pretty = \case
    TVar v -> pretty v
    TCon s -> s
    TArr a b -> "(" ++ pretty a ++ " -> " ++ pretty b ++ ")"
