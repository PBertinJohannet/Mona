module Pretty where

import Control.Arrow
import RecursionSchemes

class Pretty a where
  pretty :: a -> String

instance Pretty a => Pretty [a] where
  pretty = fmap pretty >>> unwords

instance Pretty Char where
  pretty = (:[])

class Functor f => PrettyHisto f where
  prettyH :: CVAlgebra f String

instance PrettyHisto f => Pretty (Term f) where
  pretty = histo prettyH

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (i, j, k) = pretty i ++ "\n" ++ pretty j ++ "\n" ++ pretty k ++ "\n"

inParen s = "(" ++ s ++ ")"
