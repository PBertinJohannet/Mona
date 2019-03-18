{-# LANGUAGE LambdaCase #-}

module Syntax where
import Pretty
import Control.Arrow
import Type

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Integer
  | Case Expr [Expr]
  | Fix Expr
  deriving (Show, Eq, Ord)



data Field = FieldS String | FieldApp Field Field deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

type Decl = (String, Statement)
type ExprDecl = (String, Expr)
type ClassDecl = (String, String, [(String, Scheme)]);
type InstDecl = (String, Type, [(String, Expr)]);

data Statement
  = Expr Expr
  | TypeDecl [String] Expr
  | Class String String [(Name, Scheme)]
  | Inst String Type [(Name, Expr)]
  | Sig Scheme deriving (Show, Eq, Ord);

data Program = Program{
  exprs :: [ExprDecl],
  datas :: [(String, [String], Expr)],
  clasdecls :: [ClassDecl],
  instances :: [InstDecl],
  signatures :: [(String, Scheme)]} deriving Eq

sepDecls :: [Decl] -> Program
sepDecls [] = Program [] [] [] [] []
sepDecls (d:ds) =
  let prog = sepDecls ds in
  case d of
    (n, Inst s t e) -> prog{instances = (s, t, e) : instances prog}
    (s, TypeDecl tvars e) -> prog{datas = (s, tvars, e): datas prog}
    (s, Expr e) -> prog{exprs = (s, e): exprs prog}
    (s, Class nm vr sigs) -> prog{clasdecls = (nm, vr, sigs): clasdecls prog}
    (s, Sig e) -> prog{signatures = (s, e): signatures prog}


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

sepCallee :: Expr -> (String, Expr)
sepCallee = \case
  App (Var a) b -> (a, b)
  App a b -> let (n, e) = sepCallee a in (n, App e b)

instance Pretty Expr where
  pretty e = "(" ++ (case e of
    Var n -> n
    App (App (Var "|") a) b -> pretty a ++ " | " ++ pretty b
    App a b -> pretty a ++ " " ++ pretty b
    Lam n e -> "\\"++ n ++ " -> " ++ pretty e
    Lit l -> show l
    Fix e -> "fix " ++ pretty e
    Case e ex -> "case " ++ pretty e ++ " of " ++ unlines (pretty <$> ex)
    ) ++ ")"
