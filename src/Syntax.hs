{-# LANGUAGE LambdaCase #-}

module Syntax where
import Pretty
import Control.Arrow

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit
  | Fix Expr
  deriving (Show, Eq, Ord)



data Field = FieldS String | FieldApp Field Field deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

data Program = Program [Decl] Expr deriving Eq

type Decl = (String, Statement)
type ExprDecl = (String, Expr)

data Statement = Expr Expr | TypeDecl [String] Expr deriving (Show, Eq, Ord);

-- apply a function to the leftmost element of a succession of applications.
mapLeft :: (Expr -> Expr) -> Expr -> Expr
mapLeft f = \case
  App a b -> App (mapLeft f a) b
  e -> f e

-- apply a function to the leftmost element of a succession of applications.
leftMost :: Expr -> Expr
leftMost = \case
  App a b -> leftMost a
  e -> e

leftMostVar :: Expr -> String
leftMostVar e = let Var v = leftMost e in v

uncurryCall :: Expr -> [Expr]
uncurryCall = \case
  App a b -> uncurryCall a ++ [b]
  e -> [e]

instance Pretty Expr where
  pretty e = "(" ++ (case e of
    Var n -> n
    App (App (Var "|") a) b -> pretty a ++ " | " ++ pretty b
    App a b -> pretty a ++ " " ++ pretty b
    Lam n e -> "\\"++ n ++ " -> " ++ pretty e
    Lit l -> show l
    Fix e -> "fix " ++ pretty e
    ) ++ ")"
