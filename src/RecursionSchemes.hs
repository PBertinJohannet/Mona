{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module RecursionSchemes where

import Control.Arrow
import Control.Monad
import Control.Monad.Identity
import Data.List (partition)


(&) x f = f x

newtype CofreeM f m a = CM (m (a, f (CofreeM f m a)))

type Cofree f a = CofreeM f Identity a;

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

futu :: Functor f => CVCoAlgebra f a -> a -> Term f
futu f = In <<< fmap worker <<< f
  where worker (Automatic a) = futu f a
        worker (Manual m) = In (fmap worker m)

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

-- cofree functions

(<:>) :: Monad m => m a -> m (f (CofreeM f m a)) -> CofreeM f m a
a <:> b = CM $ (,) <$> a <*> b

(<:<) :: Monad m => m a -> f (CofreeM f m a) -> CofreeM f m a
a <:< b = CM $ (, b) <$> a

(>:>) :: Monad m => a -> m (f (CofreeM f m a)) -> CofreeM f m a
a >:> b = CM $ (a,) <$> b

(<:) :: a -> f (Cofree f a) -> Cofree f a
a <: b = CM $ return (a, b)

(>:) = flip (<:)

unwrapM :: Monad m => CofreeM f m a -> m (f (CofreeM f m a))
unwrapM (CM a) = snd <$> a

unwrap :: Cofree f a -> f (Cofree f a)
unwrap = unwrapM >>> runIdentity

forgetM :: (Monad m, Functor f, Traversable f) => CofreeM f m a -> m (Term f)
forgetM = unwrapM >>> fmap (traverse forgetM) >>> join >>> fmap In

forget :: (Functor f, Traversable f) => Cofree f a -> Term f
forget = forgetM >>> runIdentity

sepM :: CofreeM f m a -> m (a, f (CofreeM f m a))
sepM (CM a) = a

sep :: Cofree f a -> (a, f (Cofree f a))
sep = sepM >>> runIdentity

sequenceCF :: (Monad m, Functor f, Traversable f)
  => CofreeM f m a -> m (Cofree f a)
sequenceCF = cataCFM (uncurry (<:) >>> return)

cataCFM :: (Monad m, Functor f, Traversable f)
  => ((b, f a) -> m a) -> CofreeM f m b -> m a
cataCFM alg = sepM >=> second (traverse (cataCFM alg)) >>> seqtup >=> alg
  where
    seqtup (a, b) = (\inB -> (a, inB)) <$> b

cataCF :: (Functor f, Traversable f)
  => ((b, f a) -> a) -> Cofree f b -> a
cataCF alg = cataCFM (alg >>> return) >>> runIdentity

anaCFM :: (Functor f, Traversable f)
  => (a -> Either (f a) (b, a)) -> b -> a -> Cofree f b
anaCFM alg def = alg >>> (keep ||| uncurry change)
  where
    keep = fmap (anaCFM alg def) >>> (def,) >>> return >>> CM
    change = anaCFM alg

instance (Traversable f, Functor f, Show (f String), Show a)
  => Show (Cofree f a) where
  show = cataCF show
