{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}


module LambdaUnt where

import Text.PrettyPrint
import RecursionSchemes
import Control.Arrow
import Data.Functor.Identity

type Name = String

data ExprF f a = Var Name
          | App a a
          | Lam (f Name) a
          | Lit Lit
          deriving (Functor, Foldable, Traversable)

type ExprNestF = ExprF Identity
type ExprFlatF = ExprF []

data Lit = Lint Int | LBool Bool deriving (Show, Eq)

type ExprNest = Term ExprNestF
type ExprFlat = Term ExprFlatF

inParen :: Doc -> Doc
inParen e = char '(' <> e <> char ')'

flattenAlg :: CVAlgebra ExprNestF ExprFlat -- f (Attr f a) -> a
flattenAlg = \case
  Lam (Identity n) (Attr (In (Lam m k)) _) -> In $ Lam (n:m) k
  Lam (Identity n) (Attr a _) -> In $ Lam [n] a
  Lit i -> In $ Lit i
  Var v -> In $ Var v
  App a b-> In $ App (attribute a) (attribute b)

nestAlg :: CoAlgebra ExprNestF ExprFlat --  ExprFlat -> ExprNestF (CoAttr f a)
nestAlg (In e) = case e of
  Lam [n] k -> Lam (Identity n) k
  Lam (n:m) k -> Lam (Identity n) (In $ Lam m k) --(Lam m _k))
  Lit i -> Lit i
  Var v -> Var v
  App a b -> App a b

flattenExpr :: ExprNest -> ExprFlat
flattenExpr = histo flattenAlg

nestExpr :: ExprFlat -> ExprNest
nestExpr = ana nestAlg

prettyfyAlg :: Algebra ExprFlatF Doc
prettyfyAlg e = case e of
  Var v -> text $ show v
  Lit (Lint i) -> text $ show i
  Lit (LBool i) -> text $ show i
  App a b -> inParen a <> inParen b
  Lam n a -> char '\\' <> hsep (text <$> n) <> text "->" <> a

class Pretty p where
  pp :: p -> Doc

instance Pretty ExprFlat where
  pp  = cata prettyfyAlg

instance Pretty ExprNest where
  pp  = pp . flattenExpr
