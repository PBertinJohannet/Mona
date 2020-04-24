{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser (
  parseModule,
) where

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Data.Text.Lazy as L
import RecursionSchemes
import Syntax

import Lexer
import Type

import Control.Arrow


type NakedStatement = StatementF Expr

toLoc :: SourcePos -> Location
toLoc s = Loc (sourceName s, sourceLine s, sourceColumn s)

--toSyntax :: SourcePos -> [Binding] -> [BindingAnn]
--toSyntax s = fmap $ second $ second $ fmap (annotate (toLoc s))

{-withPos :: Parser Expr -> Parser Expr
withPos p = do
  pos <- getPosition
  e <- p
  return $ Position pos e -}

class WithPos a where
  withPosIn :: Location -> a -> CofreeF ExprF Location (Expr)

instance WithPos Expr where
  withPosIn l (In e) = e

instance WithPos a => WithPos (ExprF a) where
  withPosIn l e' = let e = withPosIn l <$> e' in (l :< fmap In e)

withPos :: WithPos a => Location -> a -> Expr
withPos l a = In $ withPosIn l a

withPosE :: Location -> ExprF Expr -> Expr
withPosE = withPos

mapLoc :: (Location -> a -> b) -> Parser a -> Parser b
mapLoc f p = do
  pos <- toLoc <$> getPosition
  res <- p
  return (f pos res)

mkCF :: WithPos a => Parser a -> Parser Expr
mkCF = mapLoc withPos

mkCFE :: Parser (ExprF Expr) -> Parser Expr
mkCFE = mapLoc withPos

mkCFfst :: WithPos a => Parser (a, b) -> Parser (Expr, b)
mkCFfst = mapLoc $ \loc (a, b) -> (withPos loc a, b)

mkCFsnd :: WithPos b => Parser (a, b) -> Parser (a, Expr)
mkCFsnd = mapLoc $ \loc (b, a) -> (b, withPos loc a)

mkCFunc :: WithPos a => Parser (b -> a) -> Parser (b -> Expr)
mkCFunc = mapLoc $ \loc f -> ((withPos loc .) f)


posInTuple :: Parser (a, b) -> Parser (Location, a, b)
posInTuple p = do
  pos <- getPosition
  (x, y) <- p
  return (toLoc pos, x, y)

manyOne :: Parser a -> Parser (NonEmpty a)
manyOne p = do
  a <- p
  others <- many p
  return $ a :+: others

-- returns the transformed function.
pat :: Parser Pattern
pat = patternParen <|> patternId

patternId :: Parser Pattern
patternId = do
  spaces
  ident <- identifier
  spaces
  return $ Raw ident

patternParen :: Parser Pattern
patternParen = do
  spaces
  char '('
  patName <- identifier
  patterns <- many pat
  char ')'
  spaces
  return $ Pattern patName patterns

integer :: Parser Integer
integer = Tok.integer lexer

variable :: Parser Expr
variable = mkCF variable'
  where
    variable' :: Parser (ExprF Expr)
    variable' = Var <$> identifier

number :: Parser Expr
number = mkCFE $ do
  n <- integer
  return (Lit (fromIntegral n))

lst :: Parser Expr
lst = do
  loc <- toLoc <$> getPosition
  char '['
  e <- mychainlfirst expr (putIn loc) inList
  char ']'
  return e
  where
    putIn :: Location -> Expr -> Expr
    putIn loc a = withPos loc ((In (loc :< Var "Cons") `App` a) `App` Var "End")

emptylst :: Parser Expr
emptylst = mkCFE $ do
  char '['
  char ']'
  return $ Var "End"

inList :: Parser (Expr -> Expr -> Expr)
inList = do
  loca <- toLoc <$> getPosition
  let appL a b = withPos loca $ App a b
  spaces
  string ","
  spaces
  return $ \a b -> appL (appL (appL (Var "flip" :: ExprF Expr) (Var "Cons")) a) b

fix :: Parser Expr
fix = mkCF $ do
  reservedOp "fix"
  x <- expr
  return (Fix x)

lambda :: Parser Expr
lambda = mkCF $ do
  reservedOp "\\"
  args <- many pat
  reservedOp "->"
  body <- expr
  simplifyLam body args

simplifyLam :: Expr -> [Pattern] -> Parser Expr
simplifyLam body args = do
  loc <- toLoc <$> getPosition
  return $ foldr ((withPos loc .) . lamPat) body args

letin :: Parser Expr
letin = mkCF $ do
  loc <- toLoc <$> getPosition
  reserved "let"
  a <- pat
  reservedOp "="
  b <- expr
  reserved "in"
  c <- expr
  return $ App (withPos loc $ lamPat a c) b

ifthen :: Parser Expr
ifthen = mkCF $ do
  loc <- toLoc <$> getPosition
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return $ Case cond
    [PatternT (Pattern "True" []) tr,
     PatternT (Pattern "False" []) fl]

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
caseof = mkCF $ do
  reserved "case"
  e1 <- aexp
  reserved "of"
  cases <- sepBy caseline $ char ','
  return $ Case e1 cases

caseline :: Parser (PatternT Expr)
caseline = do
  spaces
  p <- pat
  spaces
  reservedOp "->"
  body <- expr
  return $ PatternT p body

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
                      k <- p
                      res <- rest k
                      return $ f a res)
                    <|> return a

mychainl :: Parser a -> Parser (a -> a -> a) -> Parser a
mychainl p = mychainlfirst p id


mychainrl :: Parser a -> Parser (a -> a -> a) -> Parser (a -> a -> a) -> Parser a
mychainrl p opr opl = do {a <- p; rest a}
  where
    rest a = more a <|> return a
    more a = left a <|> right a
    left a = do
      f <- opl
      b <- p
      rest (f a b)
    right a = do
      f <- opr
      b <- p
      res <- rest b
      return $ f a res

parseOperation :: Parser (Expr -> Expr -> Expr)
parseOperation = do
  loc <- toLoc <$> getPosition
  let appL a b = withPos loc $ App a b
  let varL a = withPosE loc $ Var a
  spaces
  symbol <- string "+"
       <|> string "-"
       <|> string "=="
       <|> string "*"
       <|> string "|"
       <|> string "."
       <|> string ""
  spaces
  return $ case symbol of
   "" -> appL
   s -> \a b ->  appL (appL (varL s) a) b

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Op a
infixOp x f = Ex.Infix (reservedOp x >> return f)

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
parseType = mychainrl ((tvar <$> identifier) <|> inParen parseType) parseArrow parseApp

inParen :: Parser a -> Parser a
inParen p = do
  char '('
  r <- p
  char ')'
  return r

parseApp :: Parser (Type -> Type -> Type)
parseApp = return TApp

parseArrow :: Parser (Type -> Type -> Type)
parseArrow = do
 spaces
 symbol <- string "->"
 spaces
 return mkArr

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
  Forall fall . Qual preds <$> parseType

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
  args <- many pat
  reservedOp "="
  body <- expr
  ret <- simplifyLam body args
  return (name, ret)

consDecl :: Parser (String, Type)
consDecl = do
  reserved "|"
  name <- identifier
  reserved "="
  x <- parseType
  optional semi
  return (name, x)

type NakedBinding = (String, NakedStatement)
type Binding = (String, Statement)

instdecl :: Parser NakedBinding
instdecl = do
  reserved "inst"
  tp <- parseType
  reserved "of"
  cls <- identifier
  reservedOp "="
  reservedOp "{"
  vals <- many $ do {x <- inLet; semi; return x}
  reservedOp "}"
  return ("", Inst cls tp vals)

sig :: Parser NakedBinding
sig = second Sig <$> sigIn

classdecl :: Parser NakedBinding
classdecl = do
  reserved "class"
  name <- identifier
  typename <- identifier
  reservedOp "="
  reservedOp "{"
  sigs <- many $ posInTuple $ do {x <- sigIn; semi; return x}
  reservedOp "}"
  return (name, Class name typename sigs)

letdecl :: Parser NakedBinding
letdecl = do
  (n, e) <- inLet
  return (n, Expr e)

typedecl :: Parser NakedBinding
typedecl = do
  reserved "data"
  name <- identifier
  tvars <- many identifier
  reservedOp "="
  body <- manyOne consDecl
  return (name, TypeDecl tvars body)

decl :: Parser NakedBinding
decl = try letdecl <|> typedecl <|> sig <|> classdecl <|> instdecl

top :: Parser Binding
top = do
  pos <- getPosition
  (name, s) <- decl
  optional semi
  return (name, (toLoc pos, s))

modl ::  Parser [Binding]
modl = many top

parseModule ::  FilePath -> L.Text -> Either ParseError [Binding]
parseModule = parse (contents modl)
