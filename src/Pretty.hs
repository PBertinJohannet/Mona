{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty where

import Control.Arrow
import RecursionSchemes

class Pretty a where
  pretty :: a -> String

class Functor f => PrettyHisto f where
  prettyH :: CVAlgebra f String

inParen s = "(" ++ s ++ ")"

instance (PrettyHisto f, Pretty a) => Pretty (Cofree f a) where
  pretty = histoCF' prettyH pretty'
    where pretty' (a, b) = b -- ++ inParen (pretty a)

prettyL :: Pretty a => [a] -> String
prettyL a = unwords (pretty <$> a)
