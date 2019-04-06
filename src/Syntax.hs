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
  | Lit Integer
  | Case a [a]
  | Fix a
  deriving (Eq, Ord, Functor, Foldable, Traversable)

type TExpr = Cofree ExprF (Location, Type); -- expression with type information (after type inference)
type Expr = Cofree ExprF Location; -- expression with position information (after the parsing)
type Forgot = Term ExprF -- expression without any information

data Field = FieldS String | FieldApp Field Field deriving (Show, Eq, Ord)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

type Decl = (String, Statement)
type ExprDecl = (String, Expr)
type ClassDecl = (String, String, [(String, Scheme)]);
type InstDecl = (String, Type, [(String, Expr)]);
type InstCheck = (String, Scheme, Expr)

instance Pretty InstCheck where
  pretty (n, sc, ex) = n ++ " :: " ++ pretty sc ++ inParen (pretty ex)

data StatementF a
 = Expr a
 | TypeDecl [String] a
 | Class String String [(String, Scheme)]
 | Inst String Type [(String, a)]
 | Sig Scheme
 deriving (Functor, Foldable, Traversable, Show)

type Statement = StatementF Expr;

data Program = Program{
  exprs :: [ExprDecl],
  datas :: [(String, [String], Expr)],
  clasdecls :: [ClassDecl],
  instances :: [InstDecl],
  signatures :: [(String, Scheme)]}

prettyDatas :: [(String, [String], Expr)] -> String
prettyDatas = unwords . fmap (\(a, b, c) -> a ++ " " ++ unwords b ++ " = " ++ pretty c)

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

instance Pretty (Location, Type) where
  pretty (l, t) = pretty t ++ " at " ++ pretty l

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
       prettycls (a, b, l) = a ++ " : " ++ b  ++ " -> " ++ prettyL l ++ "\n"
