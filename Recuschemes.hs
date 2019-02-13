{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Main where

import Control.Arrow
import Data.List (partition)
import Prelude hiding (lookup)
import System.Random

data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)

main = putStrLn "hello, world"

(&) x f = f x

-- first make it more general

data ExprF a
  = Index a a
  | For [a] a
  | Call a [a]
  | Unary String a
  | Binary a String a
  | Paren a
  | Literal Lit
  deriving (Show, Eq, Functor, Foldable, Traversable)

newtype Term f = In { out :: f (Term f) }

type Expr = Term ExprF

topDown, bottomUp :: Functor f => (Term f -> Term f) -> Term f -> Term f
bottomUp fn = out >>> fmap (bottomUp fn) >>> In >>> fn
topDown fn = In <<< fmap (bottomUp fn) <<< out <<< fn

flattenTerm :: Expr -> Expr
flattenTerm (In (Paren e)) = e  -- remove all Parens
flattenTerm other = other       -- do nothing otherwise

flatten :: Expr -> Expr
flatten = bottomUp flattenTerm

type Algebra f a = f a -> a
type CoAlgebra f a = a -> f a

countNodes :: Algebra ExprF Int

countNodes (Unary _ arg)         = arg + 1
countNodes (Binary left _ right) = left + right + 1
countNodes (Call fn args)        = fn + sum args + 1
countNodes (Index it idx)        = it + idx + 1
countNodes (Paren arg)           = arg + 1
countNodes (Literal _) = 1

cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f

ana :: (Functor f) => CoAlgebra f a -> a -> Term f
ana f = In <<< fmap (ana f) <<< f

ten, add, call, id', idten :: Expr
ten  = In (Literal (IntLit 10))
add  = In (Literal (StrLit "add"))
id'  = In (Literal (StrLit "id"))
idten = In (Call id' [In $ Call add [ten, ten]])---add(10, 10)
call = In (Call add [idten, ten])---add(10, 10)

type RAlgebra f a = Term f -> f a -> a
type RCoalgebra f a = a -> f (Either (Term f) a)

para :: Functor f => RAlgebra f a -> Term f -> a
para alg t = out t & fmap (para alg) & alg t

apo :: Functor f => RCoalgebra f a -> a -> Term f
apo f = In <<< fmap (id ||| apo f) <<< f

removeId :: RAlgebra ExprF Int
removeId (In (Call (In (Literal (StrLit "id"))) _)) (Call _ [a])  = a
removeId _ x = countNodes x


data Attr f a = Attr
              { attribute :: a
              , hole      :: f (Attr f a)
              }

data CoAttr f a = Automatic a
                | Manual (f (CoAttr f a))


type CVAlgebra f a = f (Attr f a) -> a
type CVCoAlgebra f a = a -> f (CoAttr f a)

histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = worker >>> attribute
  where worker = out >>> fmap worker >>> (h &&& id) >>> uncurry Attr

futu :: Functor f => CVCoAlgebra f a -> a -> Term f
futu f = In <<< fmap worker <<< f
  where worker (Automatic a) = futu f a
        worker (Manual m) = In (fmap worker m)

-- Fibonacci using histomorphisms.

fibo :: CVAlgebra Maybe Int
fibo Nothing = 1
fibo (Just Attr{hole = Nothing}) = 1
fibo (Just Attr{attribute = x, hole = (Just a)}) = x + attribute a

fromNumA :: CoAlgebra Maybe Int
fromNumA 0 = Nothing
fromNumA a = Just a

fromNum :: Int -> Term Maybe
fromNum = ana fromNumA

toNumA :: Algebra Maybe Int
toNumA Nothing = 0
toNumA (Just a) = a + 1

toNum :: Term Maybe -> Int
toNum = cata toNumA

fib :: Int -> Int
fib = histo fibo . fromNum

-- CCC Change using histomorphisms.

type Cent = Int

coins :: [Cent]
coins = [50, 20, 10, 5, 2, 1]

compress :: CVAlgebra Maybe Int
compress Nothing              = 0
compress (Just (Attr _ x)) = 1 + compress x

changeA :: CVAlgebra Maybe Int
changeA Nothing = 1
changeA curr@(Just attr) = let
    given               = compress curr
    validCoins          = filter (<= given) coins
    remaining           = map (given -) validCoins
    (zeroes, toProcess) = partition (== 0) remaining
    results             = sum (map (lookup attr) toProcess)
    in length zeroes + results

lookup :: Attr Maybe Int -> Int -> Int
lookup cache 0 = attribute cache
lookup cache n = lookup inner (n - 1) where (Just inner) = hole cache

change :: Cent -> Int
change = histo changeA . fromNum
