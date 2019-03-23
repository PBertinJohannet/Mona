{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module HMParser (
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

import HMLexer
import Type

import Control.Arrow

data Expr
  = Var String
  | App Expr Expr
  | Lam String Expr
  | Lit Integer
  | Case Expr [Expr]
  | Fix Expr
  deriving (Eq, Ord)

type Statement = S.StatementF Expr

toSyntax :: Statement -> S.Statement
toSyntax = fmap fixExpr

fixExpr :: Expr -> S.Expr
fixExpr = ana $ \case
  Var n -> S.Var n
  App a b -> S.App a b
  Lam a b -> S.Lam a b
  Lit i -> S.Lit i
  Case a b -> S.Case a b
  Fix a -> S.Fix a

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
  where putIn a = App (App (Var ":") a) $ Var "[]"

emptylst :: Parser Expr
emptylst = do
  char '['
  char ']'
  return $ Var "[]"

inList :: Parser (Expr -> Expr -> Expr)
inList =
  do spaces
     string ","
     spaces
     return $ \a b -> App (App (App (Var "flip") (Var ":")) a) b

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
  return (App (App (App (Var "if") cond) tr) fl)



aexp :: Parser Expr
aexp =
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
     symbol <- try (string "++")
           <|> string "+"
           <|> string "-"
           <|> string "=="
           <|> string "*"
           <|> string ":"
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
parseType = mychainr ((tvar <$> identifier) <|> inParen parseType) parseArrow

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

inSig :: Parser (String, Scheme)
inSig = do
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

type Binding = (String, Statement)

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
sig = second S.Sig <$> inSig

classdecl :: Parser Binding
classdecl = do
  reserved "class"
  name <- identifier
  typename <- identifier
  reservedOp "="
  reservedOp "{"
  sigs <- many $ do {x <- inSig; semi; return x}
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
  body <- expr
  return (name, S.TypeDecl tvars body)

constructor :: Parser Expr
constructor = do
  name <- identifier
  e <- option (Var "") expr
  return $ case e of
    Var "" -> Var name
    e -> App (Var name) e

decl :: Parser Binding
decl = try letrecdecl <|> letdecl <|> typedecl <|> sig <|> classdecl <|> instdecl

top :: Parser Binding
top = do
  x <- decl
  optional semi
  return x

modl ::  Parser [Binding]
modl = many top

parseModule ::  FilePath -> L.Text -> Either ParseError [Binding]
parseModule = parse (contents modl)
