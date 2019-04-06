{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module RecursionSchemes where

import Control.Arrow
import Control.Monad
import Control.Monad.Identity
import Data.List (partition)


(&) x f = f x

data CofreeF f a b = a :< f b deriving (Show, Eq);

type Cofree f a = Term (CofreeF f a)

instance Functor f => Functor (CofreeF f a) where
  fmap f (a :< k) = a :< fmap f k

newtype Term f = In { out :: f (Term f) }

data Attr f a = Attr
              { value :: a
              , hole      :: f (Attr f a)
              }
data CoAttr f a = Automatic a
                | Manual (f (CoAttr f a))

type Algebra f a = f a -> a
type CoAlgebra f a = a -> f a

type RAlgebra f a = Term f -> f a -> a
type RCoalgebra f a = a -> f (Either (Term f) a)

type CVAlgebra f a = f (Attr f a) -> a
type CVCoAlgebra f a = a -> f (CoAttr f a)

type GAlgebra f a b = f (b, a) -> a
type GCoAlgebra f a b = a -> f (Either b a)

type NatTrans f g = forall a . f a -> g a

inOrig :: Functor f => f (Attr f a) -> Term f
inOrig = fmap orig >>> In

orig :: Functor f => Attr f a -> Term f
orig = ana hole

reShape :: (forall a . f a -> f a) -> Term f -> Term f
reShape nat = out >>> nat >>> In

-- possibility to use the f (m a) and the f a
bicataM :: (Traversable f, Functor f, Monad m)
  => (f (m a) -> m (f a)) -> (f a -> m a) -> Term f -> m a
bicataM malg alg = out >>> fmap (bicataM malg alg) >>> malg >=> alg

cataM :: (Traversable f, Functor f, Monad m)
  => (f a -> m a) -> Term f -> m a
cataM alg = out >>> fmap (cataM alg) >>> sequence >=> alg

cata :: Functor f => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f

ana :: Functor f => CoAlgebra f a -> a -> Term f
ana f = In <<< fmap (ana f) <<< f

para :: Functor f => RAlgebra f a -> Term f -> a
para alg t = out t & fmap (para alg) & alg t

apo :: Functor f => RCoalgebra f a -> a -> Term f
apo f = In <<< fmap (id ||| apo f) <<< f

histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = worker >>> value
  where worker = out >>> fmap worker >>> (h &&& id) >>> uncurry Attr

zygo :: Functor f => Algebra f b -> GAlgebra f a b -> Term f -> a
zygo alg galg = snd . zygoHelper
  where zygoHelper = out >>> fmap zygoHelper >>> (alg . fmap fst &&& galg)

gapo :: Functor f => CoAlgebra f b -> GCoAlgebra f a b -> a -> Term f
gapo calg gcalg = In <<< fmap (ana calg ||| gapo calg gcalg) <<< gcalg

prepro :: Functor f => NatTrans f f -> Algebra f a -> Term f -> a
prepro n alg = out >>> fmap (prepro n alg) >>> n >>> alg

postpro :: Functor f => NatTrans f f -> CoAlgebra f a -> a -> Term f
postpro n calg = In <<< fmap (postpro n calg) <<< calg

topDown, bottomUp :: Functor f => (Term f -> Term f) -> Term f -> Term f
bottomUp fn = out >>> fmap (bottomUp fn) >>> In >>> fn
topDown fn = In <<< fmap (bottomUp fn) <<< out <<< fn

class Functor f => ShowAlg f where
  prettyA :: RAlgebra f String

instance (Functor f, Show (f String)) => Show (Term f) where
  show = cata show

-- cofree instances

unwrap :: Cofree f a -> f (Cofree f a)
unwrap (In (a :< b)) = b

forget :: (Functor f, Traversable f) => Cofree f a -> Term f
forget = unwrap >>> fmap forget >>> In

sep :: Cofree f a -> (a, f (Cofree f a))
sep (In (a :< b)) = (a, b)

asTup :: ((a, f (Cofree f a)) -> (b, f (Cofree f b))) -> Cofree f a -> Cofree f b
asTup f = sep >>> f >>> uncurry (:<) >>> In

rmap f = asTup $ second f
extract = snd . sep
ann = fst . sep

cataCF :: (Functor f, Traversable f)
  => ((b, f a) -> a) -> Cofree f b -> a
cataCF alg = sep >>> second (fmap (cataCF alg)) >>> alg

cataCF' :: (Functor f, Traversable f)
  => Algebra f a -> ((b, a) -> a) -> Cofree f b -> a
cataCF' alg comb = cataCF (second alg >>> comb)

anaCF :: (Functor f)
  => (a -> Either (f a) (b, a)) -> b -> a -> Cofree f b
anaCF alg def = alg >>> (keep ||| uncurry (anaCF alg))
  where
    keep = fmap (anaCF alg def) >>> (def :<) >>> In

paraCF :: Functor f => (((c, f (Cofree f c)), f b) -> b) -> Cofree f c -> b
paraCF alg = sep >>> (id &&& apply) >>> alg -- id &&& fmap (paraCF alg) >>> alg
  where apply = snd >>> fmap (paraCF alg)

histoCF :: (Functor f)
  => ((b, f (Attr f a)) -> a) -> Cofree f b -> a
histoCF alg = worker >>> value
  where
    worker = sep >>> second (fmap worker) >>> (snd &&& alg) >>> uncurry (flip Attr)

histoCF' :: (Functor f)
  => CVAlgebra f a -> ((b, a) -> a) -> Cofree f b -> a
histoCF' alg comb = histoCF (second alg >>> comb)

applyM :: (Traversable f, Functor f, Monad m)
  => (f (m a) -> m a) -> Cofree f b -> m (Cofree f (b, a))
applyM alg = sep
  >>> second (fmap (applyM alg)
    >>> (id &&& (fmap (fmap $ snd . ann) >>> alg))
    >>> uncurry mergeAnn)
  >>> seqTup
  >>> fmap (\(b, (x, a)) -> In ((b, a) :< x))

mergeAnn :: (Traversable f, Functor f, Monad m)
  => f (m (Cofree f (b, a))) -> m a -> m (f (Cofree f (b, a)), a)
mergeAnn bs added = seqTup . (,added) =<< sequence bs

mt a = (1,) <$> a

seqTup :: Applicative m => (a, m b) -> m (a, b)
seqTup (a, mb) = flip (,) <$> mb <*> pure a
