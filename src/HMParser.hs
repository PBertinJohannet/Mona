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
  return (Lit (LInt (fromIntegral n)))

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
  <|> bool
  <|> number
  <|> ifthen
  <|> fix
  <|> letin
  <|> lambda
  <|> variable
  <|> try lst
  <|> emptylst

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

inSpaces :: Parser a -> Parser a
inSpaces a = do
  spaces
  x <- a
  spaces
  return x

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
  body <- sepBy constructor $ inSpaces (char '|')
  return (name, TypeDecl tvars $ desugarData name tvars $ getCons name <$> body)

-- given MF a | MFW b returns (|) (\MF -> a) (\MFW -> b)
constructor :: Parser Expr
constructor = option (Var "()") expr

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
