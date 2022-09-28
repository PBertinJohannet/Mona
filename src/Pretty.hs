{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty where

import Control.Arrow
import RecursionSchemes
import qualified Data.Map as Map
import Data.List (intercalate)

class Pretty a where
  pretty :: a -> String

class Functor f => PrettyHisto f where
  prettyH :: CVAlgebra f String

inParen s = "(" ++ s ++ ")"

instance (PrettyHisto f, Pretty a) => Pretty (Cofree f a) where
  pretty = histoCF' prettyH pretty'
    where pretty' (a, b) = b ++ "`" ++ pretty a++"`"

prettyL :: Pretty a => [a] -> String
prettyL a = intercalate "," (pretty <$> a)

prettyM :: Pretty a => Map.Map String a -> String
prettyM a = unwords (showAssoc <$> Map.toList a)
  where showAssoc (s, a) = s ++ " : " ++ pretty a ++ "\n"
