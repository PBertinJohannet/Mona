{-# LANGUAGE FlexibleContexts #-}
module Error where

import Pretty
import Syntax
import Control.Monad.Except
import Data.List (intercalate)


data CompilerError a = CompilerError a [Location];

instance Pretty a => Pretty (CompilerError a) where
  pretty (CompilerError v []) = pretty v ++ " at <no location info>"
  pretty (CompilerError v loc) = pretty v ++ " at " ++ intercalate " at " (pretty <$> loc)

throwErrorV :: MonadError (CompilerError a) m => a -> m b
throwErrorV variant = throwError (CompilerError variant [])

withErrorLoc :: MonadError (CompilerError a) m => m b -> Location -> m b
withErrorLoc a loc = a `catchError` withLoc
  where
    withLoc (CompilerError variant a) = throwError $ CompilerError variant (loc:a)
