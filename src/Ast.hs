{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Ast where

import Control.Arrow
import Data.List (partition)
import Prelude 
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
