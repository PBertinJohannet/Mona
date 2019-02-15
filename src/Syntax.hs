module Syntax where
import Pretty

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit
  | Fix Expr
  deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

data Program = Program [Decl] Expr deriving Eq

type Decl = (String, Expr)

instance Pretty Expr where
  pretty e = "(" ++ (case e of
    Var n -> n
    App a b -> pretty a ++ " " ++ pretty b
    Lam n e -> "\\"++ n ++ " " ++ pretty e
    Lit l -> show l
    Fix e -> "fix " ++ pretty e
    ) ++ ")"
