
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}

module Type where
import Pretty
import Control.Arrow
import Data.List
import qualified Data.Set as Set
import Control.Monad
import RecursionSchemes
import Control.Applicative 
import Data.Maybe (isNothing, isJust, catMaybes, fromMaybe)

data TVar = TV{name :: String, kind :: Kind}
  deriving (Show, Eq, Ord)

data Kind = KVar String | Star | Kfun Kind Kind deriving (Show, Eq, Ord)

data Qual a = Qual{preds :: [Pred], head :: a} deriving (Show, Eq, Ord)

data Pred = IsIn String Type deriving (Show, Eq, Ord)


data Type
  = TVar TVar
  | TCon String Kind
  | TApp Type Type deriving (Show, Eq, Ord)

data BTree a = BTree (BTree a, BTree a) | Leaf a deriving (Show, Eq, Ord, Functor, Traversable, Foldable, Applicative)

instance Pretty a => Pretty (BTree a) where
  pretty = \case
    BTree (a, b) -> "{" ++ pretty a ++ ", " ++ pretty b ++ "}"
    Leaf a -> pretty a

instance Pretty () where
  pretty _ = "*"

type Class = ([String], [Inst])
type Inst = Qual Pred


-- forall a b c . a -> b
data Scheme = Forall [TVar] (Qual Type)
  deriving (Show, Eq)

mapPred :: (Type -> Type) -> Pred -> Pred
mapPred f (IsIn s a) = IsIn s $ f a 

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

data NonEmpty a = a :+: [a] deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

foldNE :: (Monad m) => NonEmpty a -> (a -> a -> m a) -> m a
foldNE (x :+: xs) f = foldM f x xs

unzipNonEmpty :: NonEmpty (a, b) -> (NonEmpty a, NonEmpty b)
unzipNonEmpty l = (fst <$> l, snd <$> l) 

sepTail :: NonEmpty a -> (a, [a])
sepTail (x :+: xs) = (x, xs)

asList :: NonEmpty a -> [a]
asList (a :+: b) = a : b

lastSafe :: NonEmpty a -> a
lastSafe = \case
  a :+: [] -> a
  a :+: [b] -> b
  a :+: (b:bs) -> lastSafe (b :+: bs)

lenMinusOne :: NonEmpty a -> Int
lenMinusOne (a :+: b) = length b

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


data ArrowType = ArrowType Type Type deriving (Eq, Show)

asType :: ArrowType -> Type
asType (ArrowType a b) = mkArr a b

type Replacement = (Type, Type);

type Replacements = [Replacement];

class Replacable a where
  replaceType :: Replacement -> a -> a

instance Replacable Type where
  replaceType (from, to) source | source == from = to
  replaceType (from, to) (TApp a b) = TApp (replaceType (from, to) a) (replaceType (from, to) b)
  replaceType _ s = s

instance Pretty Replacement where
  pretty (a, b) = "(" ++ pretty a ++ ", " ++ pretty b ++ ")"

replaceAll :: Replacable a => Replacements -> a -> a
replaceAll rep cons = foldr replaceType cons rep

instance Replacable ArrowType where
  replaceType rep (ArrowType a b) = ArrowType (replaceType rep a) (replaceType rep b)

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

depth :: Type -> Int
depth = \case
  TApp a b -> 1 + max (depth a) (depth b)
  _ -> 1

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

sepTypes :: Type -> [Type]
sepTypes t = let (a :+: as) = unapply t in a:as

unapply :: Type -> NonEmpty Type
unapply = \case
  TApp a b -> let (base :+: args) = unapply a in base :+: (args ++ [b])
  e -> e :+: []

uncurryType :: Type -> [Type]
uncurryType = \case
  TApp (TApp (TCon "(->)" k) a) b -> a : uncurryType b
  e -> return e

reApply :: NonEmpty Type -> Type
reApply (x :+: xs) = foldr TApp x xs

asTree :: Type -> BTree ()
asTree = \case
  TApp a b -> BTree (asTree a, asTree b)
  t -> Leaf ()

tvTreeToType :: BTree TVar -> Type
tvTreeToType = treeToType . fmap TVar

treeToType :: BTree Type -> Type
treeToType = \case
  BTree (a, b) -> TApp (treeToType a) (treeToType b)
  (Leaf a) -> a

treeToList :: BTree a -> [] a
treeToList = foldr (:) []

getSubStruct :: ([Maybe (BTree a, BTree a)] -> [(BTree a, BTree a)]) -> [BTree a] -> BTree ()
getSubStruct f tps = 
  case f (sep <$> tps) of
    [] -> Leaf () 
    l -> BTree (getSubStruct f `mapTuple` unzip l)
  where
    sep = \case 
      BTree (a, b) -> Just (a, b)
      _ -> Nothing

getMinimalStruct :: [BTree ()] -> BTree ()
getMinimalStruct = getSubStruct (fromMaybe [] . sequence)

getMaximalStruct :: [BTree ()] -> BTree ()
getMaximalStruct = getSubStruct catMaybes


mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

unzipF :: Functor f => f (a, b) -> (f a, f b)
unzipF = fmap fst &&& fmap snd

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

instance Pretty Pred  where
  pretty (IsIn i t) = "(" ++ i ++ " " ++ pretty t ++ ")"

instance (Pretty t) => Pretty (Qual t) where
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
    TApp a b@(TCon _ _) -> pretty a ++ " " ++ pretty b
    TApp a b@(TVar _) -> pretty a ++ " " ++ pretty b
    TApp a b -> pretty a ++ " " ++ "(" ++ pretty b ++ ")"

instance Pretty (TVar, Type) where
  pretty (a, b) = "(" ++ pretty a ++ " : " ++ pretty b ++ ")"

instance Pretty (Type, TVar) where
  pretty (a, b) = "(" ++ pretty a ++ " : " ++ pretty b ++ ")"

instance Pretty (Maybe (Type, Type)) where
  pretty = show . \case
    Just (a, b) -> Just (pretty a, pretty b)
    Nothing -> Nothing 

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
