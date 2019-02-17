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


newtype TypeDecl = Or [Prod] deriving (Show, Eq, Ord)

data Prod = Prod{name :: String, fields :: [Field]} deriving (Show, Eq, Ord)

data Field = FieldS String | FieldApp Field [Field] deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

data Program = Program [Decl] Expr deriving Eq

type Decl = (String, Statement)
type ExprDecl = (String, Expr)

data Statement = Expr Expr | TypeDecl [String] TypeDecl deriving (Show, Eq, Ord);

instance Pretty Expr where
  pretty e = "(" ++ (case e of
    Var n -> n
    App a b -> pretty a ++ " " ++ pretty b
    Lam n e -> "\\"++ n ++ " " ++ pretty e
    Lit l -> show l
    Fix e -> "fix " ++ pretty e
    ) ++ ")"
