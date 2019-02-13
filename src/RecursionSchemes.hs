module RecursionSchemes where

import Control.Arrow
import Data.List (partition)


(&) x f = f x

newtype Term f = In { out :: f (Term f) }

topDown, bottomUp :: Functor f => (Term f -> Term f) -> Term f -> Term f
bottomUp fn = out >>> fmap (bottomUp fn) >>> In >>> fn
topDown fn = In <<< fmap (bottomUp fn) <<< out <<< fn

data Attr f a = Attr
              { attribute :: a
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

type NatTrans f g a = f a -> g a

cata :: Functor f => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f

ana :: Functor f => CoAlgebra f a -> a -> Term f
ana f = In <<< fmap (ana f) <<< f

para :: Functor f => RAlgebra f a -> Term f -> a
para alg t = out t & fmap (para alg) & alg t

apo :: Functor f => RCoalgebra f a -> a -> Term f
apo f = In <<< fmap (id ||| apo f) <<< f

histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = worker >>> attribute
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

prepro :: Functor f => NatTrans f f a -> Algebra f a -> Term f -> a
prepro n alg = out >>> fmap (prepro n alg) >>> n >>> alg

postpro :: Functor f => NatTrans f f a -> CoAlgebra f a -> a -> Term f
postpro n calg = In <<< fmap (postpro n calg) <<< calg


{-
preprozygo :: Functor f => TransSelf f a -> Algebra f b -> GAlgebra f a b -> Term f -> a
preprozygo n calg galg = snd . zygoHelper
  where zygoHelper = out >>> fmap zygoHelper >>> fmap n >>> (alg . fmap fst &&& galg)
-}
