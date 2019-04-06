{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty where

import Control.Arrow
import RecursionSchemes

class Pretty a where
  pretty :: a -> String

class Functor f => PrettyHisto f where
  prettyH :: CVAlgebra f String

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (i, j, k) = pretty i ++ "\n" ++ pretty j ++ "\n" ++ pretty k ++ "\n"

inParen s = "(" ++ s ++ ")"

instance (PrettyHisto f, Pretty a) => Pretty (Cofree f a) where
  pretty = histoCF' prettyH pretty'
    where pretty' (a, b) = b -- ++ inParen (pretty a)

prettyL :: Pretty a => [a] -> String
prettyL a = unwords (pretty <$> a)
