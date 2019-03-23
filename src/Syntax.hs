{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Syntax where
import Pretty
import Control.Arrow
import RecursionSchemes
import qualified Data.Map as Map
import Type

type Name = String

data ExprF a
  = Var Name
  | App a a
  | Lam Name a
  | Lit Integer
  | Case a [a]
  | Fix a
  deriving (Eq, Ord, Functor, Foldable, Traversable)

makeConstructor :: (a -> b -> ExprF Expr) -> a -> b -> Expr
makeConstructor f a = f a >>> In

varC = Var >>> In
appC = makeConstructor App
lamC = makeConstructor Lam
litC = Lit >>> In
caseC = makeConstructor Case
fixC = Fix >>> In



type Expr = Term ExprF;


data Field = FieldS String | FieldApp Field Field deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

type Decl = (String, Statement)
type ExprDecl = (String, Expr)
type ClassDecl = (String, String, [(String, Scheme)]);
type InstDecl = (String, Type, [(String, Expr)]);

data StatementF a
 = Expr a
 | TypeDecl [String] a
 | Class String String [(String, Scheme)]
 | Inst String Type [(String, a)]
 | Sig Scheme
 deriving (Functor)

type Statement = StatementF Expr;

data Program = Program{
  exprs :: [ExprDecl],
  datas :: [(String, [String], Expr)],
  clasdecls :: [ClassDecl],
  instances :: [InstDecl],
  signatures :: [(String, Scheme)]}

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



mapLeft :: (Expr -> Expr) -> Expr -> Expr
mapLeft f = \case
  In (App a b) -> In $ App (mapLeft f a) b
  In e -> In $ fmap f e

leftMost :: Expr -> Expr
leftMost = cata $ \case
  App a b -> a
  e -> In e

leftMostVar :: Expr -> String
leftMostVar e = let Var v = out $ leftMost e in v

uncurryCall :: Expr -> [Expr]
uncurryCall = para $ curry $ \case
  (In (App _ b), App a _) -> a ++ [b]
  (e, _) -> [e]

sepCallee :: Expr -> (String, Expr)
sepCallee = out >>> \case
  App (In (Var a)) b -> (a, b)
  App a b -> let (n, e) = sepCallee a in (n, In $ App e b)

type ExprAttr a = ExprF (Attr ExprF a);
type AttrExpr a = Attr ExprF a;

isAppOfH :: String -> ExprAttr a -> Maybe (a, a)
isAppOfH s = \case
  App (Attr _ (App (Attr _ (Var s)) (Attr a _))) (Attr b _) -> Just (a, b)
  _ -> Nothing

matchApp :: AttrExpr a -> AttrExpr a -> (Maybe (String, a, a), (a, a))
matchApp a (Attr b _)= case a of
  (Attr _ (App (Attr _ (Var s)) (Attr a' _))) -> (Just (s, a', b), (value a, b))
  _ -> (Nothing, (value a, b))

instance Show (ExprF String) where
  show = inParen <<< \case
    Var n -> "Var " ++ n
    App a b -> "App " ++ a ++ " " ++ b
    Lam a b -> "Lam " ++ a ++ " " ++ b
    Case a b -> "Case " ++ a ++ " " ++ show b
    Lit n -> "Lit " ++ show n
    Fix n -> "Fix " ++ n

instance PrettyHisto ExprF where
  prettyH = inParen <<< \case
    Var n -> n
    App a b -> case matchApp a b of
      (Just ("|", a, b), _) -> a ++ "|" ++ b
      (_, (a, b)) -> unwords [a, b]
    Lam n e -> "\\" ++ n ++ " -> " ++ value e
    Lit l -> show l
    Fix e -> "fix " ++ value e
    Case e ex -> "case " ++ value e ++ " of " ++ unlines (value <$> ex)
