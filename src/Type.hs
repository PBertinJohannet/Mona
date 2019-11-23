{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Type where
import Pretty
import Control.Arrow
import Data.List

data TVar = TV{name :: String, kind :: Kind}
  deriving (Show, Eq, Ord)

data Kind = KVar String | Star | Kfun Kind Kind deriving (Show, Eq, Ord)

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

instance Pretty (TVar, Type) where
  pretty (a, b) = "(" ++ pretty a ++ " : " ++ pretty b ++ ")"

instance Pretty (Type, TVar) where
  pretty (a, b) = "(" ++ pretty a ++ " : " ++ pretty b ++ ")"

mapPred :: (Type -> Type) -> Pred -> Pred
mapPred f (IsIn s t) = IsIn s (f t)

mapKind :: (Kind -> Kind) -> Type -> Type
mapKind f = \case
  TVar (TV a k) -> TVar (TV a $ f k)
  TCon s k -> TCon s $ f k
  TApp a b -> TApp (mapKind f a) (mapKind f b)

makeTypeConstant :: String -> Type -> Type
makeTypeConstant n = \case
  TVar (TV a k) -> if a == n then TCon n k else TVar (TV a k)
  TCon s k -> TCon s k
  TApp a b -> TApp (makeTypeConstant n a) (makeTypeConstant n b)

data NonEmpty a = a :+: [a] deriving (Eq, Show, Functor, Foldable, Traversable)

asList :: NonEmpty a -> [a]
asList (a :+: b) = a : b

lastSafe :: NonEmpty a -> a
lastSafe = \case
  a :+: [] -> a
  a :+: [b] -> b
  a :+: (b:bs) -> lastSafe (b :+: bs)

lenMinusOne :: NonEmpty a -> Int
lenMinusOne (a :+: b) = length b

-- transforms a constructor's type to a pattern's type :
-- List :: a -> List a -> List a becomes ~List :: List a -> (a -> List a -> b) -> b
-- more generaly :
-- Cons :: a -> T becomes ~Cons :: (a -> b) -> T -> b
-- Cons :: a -> b -> T becomes ~Cons :: (a -> b -> c) -> (T -> c)
-- Cons :: T becomes ~Cons :: b -> T -> b
consToPat :: (String, Scheme) -> (String, Scheme)
consToPat (name, Forall t (Qual p h)) = ("~" ++ name, Forall (retVar:t) newHead)
  where
    newHead = Qual p (consToPat' (sepArgs h) [])

    consToPat' :: NonEmpty Type -> [Type] -> Type
    consToPat' (t :+: []) b = foldr mkArr retType b `mkArr` (t `mkArr` retType)
    consToPat' (a :+: (t:ts)) b = consToPat' (t :+: ts) $ b ++ [a]

    retType = TVar retVar
    retVar = TV "~'patret" Star

    mkCons :: Type -> Type -> Type
    mkCons t b = t `mkArr` (b `mkArr` b)

withPred :: String -> Pred -> Scheme -> Scheme
withPred tv p (Forall tvars (Qual q ty)) = Forall (var tv:tvars) (Qual (p:q) ty)

var :: String -> TVar
var s = TV{name = s, kind = Star}

tvar :: String -> Type
tvar s = TVar $ var s

getV :: Type -> TVar
getV (TVar t) = t

typeInt  = TCon "Int" Star
typeBool = TCon "Bool" Star
typeUnit = TCon "Unit" Star
typeChar = TCon "Char" Star
typeIO = TCon "IO" $ Kfun Star Star
typeList = TCon "List" $ Kfun Star Star
typeString = TApp typeList typeString
tArr = TCon "(->)" $ Kfun Star (Kfun Star Star)

-- used to resolve kinds
typeStar :: Type
typeStar = TCon "Star" Star

mkArr :: Type -> Type -> Type
mkArr a = TApp (TApp tArr a)

mkList :: Type -> Type
mkList = TApp typeList

setReturn :: Type -> Type -> Type
setReturn = \case
  TApp (TApp (TCon "(->)" k) a) b -> TApp (TApp (TCon "(->)" k) a) . setReturn b
  _ -> id

sepArgs :: Type -> NonEmpty Type
sepArgs = \case
  TApp (TApp (TCon "(->)" k) a) b -> a :+: asList (sepArgs b)
  e -> e :+: []

leftMostType :: Type -> Type
leftMostType = \case
  TApp a b -> leftMostType a
  t -> t

getReturn :: Type -> Type
getReturn = \case
  TApp (TApp (TCon "(->)" k) a) b -> getReturn b
  e -> e

unapply :: Type -> Type
unapply = \case
  TApp a b -> a
  e -> e

kindToFunc :: Kind -> Type
kindToFunc = \case
  Star -> tvar "a"
  Kfun a b -> TApp (TApp tArr $ kindToFunc a) $ kindToFunc b

extractKind :: Type -> Kind
extractKind = \case
  TVar (TV a _) -> Star
  TCon s k -> k
  TApp (TApp (TCon "(->)" _) a) b -> Kfun (extractKind a) (extractKind b)
  TApp a b -> Kfun (extractKind a) (extractKind b)

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
  pretty (parents, instances) =
    unwords parents ++ " => " ++ intercalate ", " (pretty <$> instances) ++ "\n"

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
    TCon s k -> s -- ++ " : " ++ pretty k
    TApp (TApp (TCon "(->)" _) a) b -> case a of
      TApp (TApp (TCon "(->)" _) _) _ ->"(" ++ pretty a ++ ") -> " ++ pretty b
      a -> pretty a ++ " -> " ++ pretty b
    TApp (TCon "List" _) a -> "[" ++ pretty a ++ "]"
    TApp a b@(TApp (TApp (TCon "(->)" _) _) _) -> pretty a ++ " (" ++ pretty b ++ ")"
    TApp a b -> pretty a ++ " " ++ pretty b

class ShowKind a where
  showKind :: a -> String

instance ShowKind a => ShowKind [a] where
  showKind = fmap showKind >>> unwords

instance ShowKind Type where
  showKind = \case
    TVar v -> showKind v
    TCon s k -> s ++ " (" ++ pretty k ++ ")"
    TApp (TApp (TCon "(->)" _) a) b -> case a of
      TApp (TApp (TCon "(->)" _) _) _ ->"(" ++ showKind a ++ ") -> " ++ showKind b
      a -> showKind a ++ " -> " ++ showKind b
    TApp (TCon "List" _) a -> "[" ++ showKind a ++ "]"
    TApp a b -> showKind a ++ " (" ++ showKind b ++ ")"

instance ShowKind TVar where
  showKind (TV v k) = v ++ "{"++ pretty k ++"}"

instance ShowKind Scheme where
  showKind = let pretty' q = unwords (pretty <$> q) in
    \case
      Forall [] (Qual q tp) -> pretty' q ++ " => " ++ showKind tp
      Forall t (Qual q tp) -> "forall " ++ unwords (fmap showKind t) ++ " . "++ pretty' q ++ " => " ++ showKind tp

instance Pretty Kind where
  pretty = \case
    KVar a -> a
    Kfun (KVar a) (KVar b) -> a ++ " -> " ++ b
    Kfun (KVar a) k -> a ++ " -> " ++ pretty k
    Star -> "*"
    Kfun Star Star -> "* -> *"
    Kfun Star k -> "* -> " ++ pretty k
    Kfun k k' -> "(" ++ pretty k ++ ") -> " ++ pretty k'
