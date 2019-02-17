module Pretty where

import Control.Arrow

class Pretty a where
  pretty :: a -> String

instance Pretty a => Pretty [a] where
  pretty = fmap pretty >>> unwords
