{-# LANGUAGE RankNTypes #-}

module Main where
import Control.Applicative
import Data.Functor.Identity

main :: IO ()
main = (hello ++ ) <$> getLine >>= putStrLn

hello :: String
hello = "hello "

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Traversal s t a b
  = forall f. Applicative f => (a -> f b) -> s -> f t

both :: Traversal (a, a) (b, b) a b
both f (x, y) = (,) <$> f x <*> f y


_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = flip (,) x <$> f a
_2 f (a, x) = (,) x <$> f a

view :: ((a -> Const a b) -> s -> Const a t) -> s -> a
view l s = getConst (l Const s)

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)
