{-# LANGUAGE OverloadedStrings #-}

module HMParser (
  parseExpr,
  parseModule
) where

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Data.Text.Lazy as L

import HMLexer
import Syntax

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
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (Let x e1 e2)

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

fromOp :: [Expr] -> Expr
fromOp (e:es) = e

inExpr :: Parser Expr
inExpr = mychainl aexp parseOperation

mychainl :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
p `mychainl` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

parseOperation :: Parser (Expr -> Expr -> Expr)
parseOperation =
  do spaces
     symbol <- string "+" <|> string "-" <|> string "==" <|> string "*" <|> string ""
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

type Binding = (String, Expr)

letdecl :: Parser Binding
letdecl = do
  reserved "let"
  name <- identifier
  args <- many identifier
  reservedOp "="
  body <- expr
  return (name, foldr Lam body args)

val :: Parser Binding
val = do
  ex <- expr
  return ("it", ex)

decl :: Parser Binding
decl = letdecl <|> val

top :: Parser Binding
top = do
  x <- decl
  optional semi
  return x

modl ::  Parser [Binding]
modl = many top

parseExpr :: L.Text -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseModule ::  FilePath -> L.Text -> Either ParseError [(String, Expr)]
parseModule = parse (contents modl)
