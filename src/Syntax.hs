{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module Syntax where
import Pretty
import Control.Arrow
import RecursionSchemes
import qualified Data.Map as Map
import Type

type Name = String

newtype Location = Loc (String, Int, Int) deriving Show;

data ExprF a
  = Var Name
  | App a a
  | Lam Name a
  | Lit Int
  | Case a [a]
  | Fix a
  deriving (Eq, Ord, Functor, Foldable, Traversable)

type Expr = Cofree ExprF Location; -- expression with position information (after the parsing)
type Forgot = Term ExprF -- expression without any information

data Field = FieldS String | FieldApp Field Field deriving (Show, Eq, Ord)

type NonEmpty a = (a, [a]);

type Decl = (String, Statement)
type ExprDecl = (Location, String, Expr)
type ClassDecl = (Location, String, String, [(Location, String, Scheme)]);
type DataDecl = (Location, String, [String], NonEmpty (String, Type))
type InstDecl = (Location, String, Type, [(String, Expr)]);
type InstCheck = ([Location], String, Scheme, Expr)

instance Pretty (Location, String, Scheme) where
  pretty (loc, n, sc) = n ++ " :: " ++ pretty sc

instance Pretty (Location, String, Expr) where
  pretty (loc, n, ex) = n ++ " :: " ++ pretty ex

instance Pretty InstCheck where
  pretty (loc, n, sc, ex) = n ++ " :: " ++ pretty sc ++ inParen (pretty ex)

data StatementF a
 = Expr a
 | TypeDecl [String] [(String, Type)]
 | Class String String [(Location, String, Scheme)]
 | Inst String Type [(String, a)]
 | Sig Scheme
 deriving (Functor, Foldable, Traversable, Show)

type Statement = (Location, StatementF Expr);

data Program = Program{
  exprs :: [ExprDecl],
  datas :: [DataDecl],
  clasdecls :: [ClassDecl],
  instances :: [InstDecl],
  signatures :: [(Location, String, Scheme)]}

prettyDatas :: [DataDecl] -> String
prettyDatas = unwords . fmap (\(loc, a, b, c) -> a ++ " (" ++ unwords b ++ ") ::\n " ++ prettyL c ++ "\n\n")

sepDecls :: [Decl] -> Program
sepDecls [] = Program [] [] [] [] []
sepDecls (d:ds) =
  let prog = sepDecls ds in
  case d of
    (n, (loc, Inst s t e)) -> prog{instances = (loc, s, t, e) : instances prog}
    (s, (loc, TypeDecl tvars e)) -> prog{datas = (loc, s, tvars, e): datas prog}
    (s, (loc, Expr e)) -> prog{exprs = (loc, s, e): exprs prog}
    (s, (loc, Class nm vr sigs)) -> prog{clasdecls = (loc, nm, vr, sigs): clasdecls prog}
    (s, (loc, Sig e)) -> prog{signatures = (loc, s, e): signatures prog}

appC :: Expr -> Expr -> Expr
appC a@(In (l :< _)) b = In $ l :< App a b

appC' :: (Location -> Expr) -> Expr -> Expr
appC' a b@(In (l :< _)) = In $ l :< App (a l) b

varC :: String -> Location -> Expr
varC s l = In $ l :< Var s

lamC :: String -> Expr -> Expr
lamC s a@(In (l :< _)) = In $ l :< Lam s a

mapLeft :: (Expr -> Expr) -> Expr -> Expr
mapLeft f = asTup $ second $ \case
  (App a b) -> App (mapLeft f a) b
  e -> fmap f e

leftMost :: Expr -> Expr
leftMost = cataCF $ \case
  (b, App a _) -> a
  (b, e) -> In $ b :< e

leftMostVar :: Expr -> String
leftMostVar e = let Var v = extract $ leftMost e in v

uncurryCall :: Expr -> [Expr]
uncurryCall = paraCF $ \case
  ((_, App _ b), App a _) -> a ++ [b]
  ((b, e), _) -> [In $ b :< e]

sepCallee :: Expr -> (String, Expr)
sepCallee = sep >>> \case
  (p, App (In (_ :< Var a)) b) -> (a, b)
  (p, App a b) -> let (n, e) = sepCallee a in (n, In $ p :< App e b)

type ExprAttr a = ExprF (Attr ExprF a);
type AttrExpr a = Attr ExprF a;

isAppOfH :: String -> ExprAttr a -> Maybe (a, a)
isAppOfH s = \case
  App (Attr _ (App (Attr _ (Var s)) (Attr a _))) (Attr b _) -> Just (a, b)
  _ -> Nothing

matchAppCF :: Attr (CofreeF ExprF b) a -> Attr (CofreeF ExprF b) a -> (Maybe (String, a, a), (a, a))
matchAppCF a (Attr b _)= case a of
  (Attr _ (_ :< App (Attr _ (_ :< Var s)) (Attr a' _))) -> (Just (s, a', b), (value a, b))
  _ -> (Nothing, (value a, b))

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

instance Pretty a => Pretty (ExprF a) where
  pretty = fmap pretty >>> show

prettyShape :: ExprF a -> String
prettyShape = \case
  Var n -> "Var " ++ n
  App a b -> "App "
  Lam a b -> "Lam " ++ a
  Case a b -> "Case "
  Lit n -> "Lit " ++ show n
  Fix n -> "Fix "

instance PrettyHisto ExprF where
  prettyH = inParen <<< \case
    Var n -> n
    App a b -> case matchApp a b of
      (Just ("|", a, b), _) -> a ++ "|" ++ b
      (_, (a, b)) -> unwords [a, b]
    Lam n e -> "/" ++ n ++ " -> " ++ value e
    Lit l -> show l
    Fix e -> "fix " ++ value e
    Case e ex -> "case " ++ value e ++ " of " ++ unlines (value <$> ex)

instance Pretty a => Pretty (String, a) where
  pretty (k, c) = k ++ " = " ++ pretty c ++ "\n"

instance Pretty Location where
  pretty (Loc (file, line, col)) = file ++ " " ++ show line ++ ":" ++ show col

instance Pretty (ClassDecl, [InstDecl]) where
  pretty (c, i) = prettycls c ++ " \n => \n "++ unwords (show <$> i) ++ "\n"
     where
       prettycls (loc, a, b, l) = a ++ " : " ++ b  ++ " -> " ++ prettyL l ++ "\n"
