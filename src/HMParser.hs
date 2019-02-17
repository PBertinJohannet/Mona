{-# LANGUAGE OverloadedStrings #-}

module HMParser (
  parseModule
) where

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Data.Text.Lazy as L

import HMLexer
import Syntax

import Control.Arrow

integer :: Parser Integer
integer = Tok.integer lexer

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

number :: Parser Expr
number = do
  n <- integer
  return (Lit (LInt (fromIntegral n)))

lst :: Parser Expr
lst = do
  char '['
  e <- mychainlfirst expr putIn inList
  char ']'
  return e
  where putIn a = App (App (Var ":") a) $ Var "[]"

inList :: Parser (Expr -> Expr -> Expr)
inList =
  do spaces
     string ","
     spaces
     return $ \a b -> App (App (App (Var "flip") (Var ":")) a) b

bool :: Parser Expr
bool = (reserved "True" >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

fix :: Parser Expr
fix = do
  reservedOp "fix"
  x <- expr
  return (Fix x)

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many identifier
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args

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
  <|> bool
  <|> number
  <|> ifthen
  <|> fix
  <|> letin
  <|> lambda
  <|> variable
  <|> lst

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

prodDecl :: Parser TypeDecl
prodDecl = do
  name <- identifier
  fields <- many (tApp <|> (: []) <$> identifier)
  return $ Or [Prod name fields]

tApp :: Parser [String]
tApp = do
  char '('
  args <- many identifier
  char ')'
  return args

inData :: Parser TypeDecl
inData = mychainl prodDecl orSep

orSep :: Parser (TypeDecl -> TypeDecl -> TypeDecl)
orSep = do
  spaces
  string "|"
  spaces
  return $ \(Or a) (Or a') -> Or (a ++ a')

type Binding = (String, Statement)

letdecl :: Parser Binding
letdecl = do
  reserved "let"
  name <- identifier
  args <- many identifier
  reservedOp "="
  body <- expr
  return (name, Expr $ foldr Lam body args)

typedecl :: Parser Binding
typedecl = do
  reserved "data"
  name <- identifier
  tvars <- many identifier
  reservedOp "="
  body <- inData
  return (name, TypeDecl tvars body)

decl :: Parser Binding
decl = letdecl <|> typedecl

top :: Parser Binding
top = do
  x <- decl
  optional semi
  return x

modl ::  Parser [Binding]
modl = many top

parseModule ::  FilePath -> L.Text -> Either ParseError [Binding]
parseModule = parse (contents modl)
