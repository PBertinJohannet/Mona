module Pretty where

import Control.Arrow

class Pretty a where
  pretty :: a -> String

instance Pretty a => Pretty [a] where
  pretty = fmap pretty >>> unwords

instance Pretty Char where
  pretty = (:[])

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (i, j, k) = pretty i ++ "\n" ++ pretty j ++ "\n" ++ pretty k ++ "\n"
