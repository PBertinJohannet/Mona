{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Parser (
  parseModule,
  toSyntax
) where

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Data.Text.Lazy as L
import RecursionSchemes
import qualified Syntax as S

import Lexer
import Type

import Control.Arrow

data Expr
  = Var String
  | App Expr Expr
  | Lam String Expr
  | Lit Int
  | Case Expr [Expr]
  | Fix Expr
  | Position SourcePos Expr
  deriving (Eq, Ord)

type Statement = S.StatementF Expr
type StatementAnn = S.StatementF S.Expr

toLoc :: SourcePos -> S.Location
toLoc s = S.Loc (sourceName s, sourceLine s, sourceColumn s)

toSyntax :: SourcePos -> [Binding] -> [BindingAnn]
toSyntax s = fmap $ second (fmap $ annotate (toLoc s))

annotate :: S.Location -> Expr -> S.Expr
annotate = anaCF myAlg

myAlg :: Expr -> Either (S.ExprF Expr) (S.Location, Expr)
myAlg = \case
  Position s e -> Right (toLoc s, e)
  e -> Left $ case e of
    Var n -> S.Var n
    App a b -> S.App a b
    Lam a b -> S.Lam a b
    Lit i -> S.Lit i
    Case a b -> S.Case a b
    Fix a -> S.Fix a

withPos :: Parser Expr -> Parser Expr
withPos p = do
  pos <- getPosition
  e <- p
  return $ Position pos e

-- returns the transformed function.
pat :: Parser (Expr -> Expr)
pat = patternId <|> patternParen

patternId :: Parser (Expr -> Expr)
patternId = do
  ident <- identifier
  return $ \e -> Lam ident e

patternParen :: Parser (Expr -> Expr)
patternParen = do
  spaces
  char '('
  patName <- identifier
  patterns <- many pat
  char ')'
  spaces
  return (App (Var $ "~" ++ patName) . foldr (.) id patterns)

integer :: Parser Integer
integer = Tok.integer lexer

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

number :: Parser Expr
number = do
  n <- integer
  return (Lit (fromIntegral n))

lst :: Parser Expr
lst = do
  char '['
  e <- mychainlfirst expr putIn inList
  char ']'
  return e
  where putIn a = App (App (Var "Cons") a) $ Var "End"

emptylst :: Parser Expr
emptylst = do
  char '['
  char ']'
  return $ Var "End"

inList :: Parser (Expr -> Expr -> Expr)
inList =
  do spaces
     string ","
     spaces
     return $ \a b -> App (App (App (Var "flip") (Var "Cons")) a) b

fix :: Parser Expr
fix = do
  reservedOp "fix"
  x <- expr
  return (Fix x)

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  inLambda

inLambda :: Parser Expr
inLambda = do
  args <- many pat
  reservedOp "->"
  body <- expr
  return $ foldr (.) id args body

letin :: Parser Expr
letin = do
  reserved "let"
  a <- identifier
  reservedOp "="
  b <- expr
  reserved "in"
  c <- expr
  return $ App (Lam a c) b

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return $ Case cond [App (Var "~True") tr, App (Var "~False") fl]

aexp :: Parser Expr
aexp = withPos $
   parens expr
  <|> number
  <|> ifthen
  <|> fix
  <|> letin
  <|> lambda
  <|> variable
  <|> try lst
  <|> emptylst
  <|> caseof

caseof :: Parser Expr
caseof = do
  reserved "case"
  e1 <- aexp
  reserved "of"
  cases <- sepBy inLambda $ char ','
  return $ Case e1 cases


fromOp :: [Expr] -> Expr
fromOp (e:es) = e

inExpr :: Parser Expr
inExpr = mychainl aexp parseOperation

mychainlfirst :: Parser a -> (a -> a) -> Parser (a -> a -> a) -> Parser a
mychainlfirst p f op = do {a <- f <$> p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a


mychainr :: Parser a -> Parser (a -> a -> a) -> Parser a
mychainr p op = do {a <- p; rest a}
  where rest a = (do  f <- op
                      b <- p
                      res <- rest b
                      return $ f a res)
                    <|> return a

mychainl :: Parser a -> Parser (a -> a -> a) -> Parser a
mychainl p = mychainlfirst p id

parseOperation =
  do spaces
     symbol <- string "+"
           <|> string "-"
           <|> string "=="
           <|> string "*"
           <|> string "|"
           <|> string "."
           <|> string ""
     spaces
     return $ case symbol of
       "" -> App
       s -> \a b -> App (App (Var s) a) b

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

makeOp :: String -> Expr -> Expr -> Expr
makeOp o a = App (App (Var o) a)

expr :: Parser Expr
expr = inExpr

inSpaces :: Parser a -> Parser a
inSpaces a = do
  spaces
  x <- a
  spaces
  return x

parsePred :: Parser Pred
parsePred = do
  cls <- identifier
  tp <- parseType
  return $ IsIn cls tp

parseType :: Parser Type
parseType = mychainl ((tvar <$> identifier) <|> inParen parseType) parseArrow

inParen :: Parser a -> Parser a
inParen p = do
  char '('
  r <- p
  char ')'
  return r

parseArrow :: Parser (Type -> Type -> Type)
parseArrow = do
  spaces
  symbol <- string "->"
       <|> string ""
  spaces
  return $ case symbol of
   "" -> TApp
   _ -> mkArr

parseForall :: Parser [TVar]
parseForall = do
  spaces
  reserved "forall"
  spaces
  names <- many identifier
  reservedOp "."
  return $ var <$> names

parseScheme :: Parser Scheme
parseScheme = do
  fall <- option [] parseForall
  preds <- try parsePreds <|> return []
  tp <- parseType
  return $ Forall fall $ Qual preds tp

parsePreds :: Parser [Pred]
parsePreds = do
  preds <- sepBy parsePred $ reservedOp ","
  reservedOp "=>"
  return preds

sigIn :: Parser (String, Scheme)
sigIn = do
  reserved "sig"
  name <- identifier
  reservedOp "="
  body <- parseScheme
  return (name, body)

inLet :: Parser (String, Expr)
inLet = do
  reserved "let"
  name <- identifier
  args <- many identifier
  reservedOp "="
  body <- expr
  return (name, foldr Lam body args)

consDecl :: Parser (String, Type)
consDecl = do
  reserved "|"
  name <- identifier
  reserved "="
  x <- parseType
  optional semi
  return (name, x)

type Binding = (String, Statement)
type BindingAnn = (String, StatementAnn)

instdecl :: Parser Binding
instdecl = do
  reserved "inst"
  tp <- parseType
  reserved "of"
  cls <- identifier
  reservedOp "="
  reservedOp "{"
  vals <- many $ do {x <- inLet; semi; return x}
  reservedOp "}"
  return ("", S.Inst cls tp vals)

sig :: Parser Binding
sig = second S.Sig <$> sigIn

classdecl :: Parser Binding
classdecl = do
  reserved "class"
  name <- identifier
  typename <- identifier
  reservedOp "="
  reservedOp "{"
  sigs <- many $ do {x <- sigIn; semi; return x}
  reservedOp "}"
  return (name, S.Class name typename sigs)

letdecl :: Parser Binding
letdecl = do
  (n, e) <- inLet
  return (n, S.Expr e)

letrecdecl :: Parser Binding
letrecdecl = do
  reserved "let"
  reserved "rec"
  name <- identifier
  arg <- many identifier
  reservedOp "="
  body <- expr
  return (name, S.Expr $ Fix $ Lam name $ foldr Lam body arg)

typedecl :: Parser Binding
typedecl = do
  reserved "data"
  name <- identifier
  tvars <- many identifier
  reservedOp "="
  body <- many consDecl
  return (name, S.TypeDecl tvars body)

decl :: Parser Binding
decl = try letrecdecl <|> letdecl <|> typedecl <|> sig <|> classdecl <|> instdecl

top :: Parser Binding
top = do
  x <- decl
  optional semi
  return x

modl ::  Parser (SourcePos, [Binding])
modl = do
  bindings <- many top
  pos <- getPosition
  return (pos, bindings)

parseModule ::  FilePath -> L.Text -> Either ParseError [BindingAnn]
parseModule = parse (contents modl) >>> fmap (fmap $ uncurry toSyntax)
