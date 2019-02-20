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

desugarData :: String -> [String] -> [(String, Expr)] -> Expr
desugarData name idents cts =
  let (names, c:cs) = unzip cts in
  let body = foldr (App . App (Var "|")) c cs in
  foldr Lam body $ idents ++ names

getCons :: String -> Expr -> (String, Expr)
getCons s e = (extractCons s e, e)

extractCons :: String -> Expr -> String
extractCons s = \case
  Lam a e -> extractCons s e
  App a b -> extractCons s a
  Var v -> v
  Fix f -> s
  Lit l -> show l

-- apply a function to the leftmost element of a succession of applications.
mapLeft :: (Expr -> Expr) -> Expr -> Expr
mapLeft f = \case
  App a b -> App (mapLeft f a) b
  e -> f e


instance Pretty Expr where
  pretty e = "(" ++ (case e of
    Var n -> n
    App a b -> pretty a ++ " " ++ pretty b
    Lam n e -> "\\"++ n ++ " -> " ++ pretty e
    Lit l -> show l
    Fix e -> "fix " ++ pretty e
    ) ++ ")"
