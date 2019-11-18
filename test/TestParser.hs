module TestParser where

import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)
import qualified Data.Text.Lazy as L
import Control.Monad.State

type Name = String

data Test
  = BaseCode Name String [Test]
  | End String SourcePos SourcePos;

-- parses a title of level at least i
parseLevelDecl :: Int -> Parser (Int, String)
parseLevelDecl i = do
  ht <- string (replicate i '#')
  ht' <- many (char '#')
  title <- many (noneOf "\n")
  char '\n'
  return (length ht + length ht', title)

-- will fail if ``` is in the code but I'll correct it when it happens
parseCode :: Parser String
parseCode = inBetween "```" "```" (many $ noneOf "`")

parseExpected :: Parser Test
parseExpected = do
  pos <- getPosition
  string ">>>"
  res <- many (noneOf "\n")
  char '\n'
  pos' <- getPosition
  return $ End res pos pos'

inBetween :: String -> String -> Parser a -> Parser a
inBetween begin end p = do
  string begin
  res <- p
  string end
  return res

comment :: Parser String
comment = many ((do
  char '`'
  noneOf "`")
  <|> (do
  string "``"
  noneOf "`")
  <|> noneOf "`#>")

parseHigherLevel :: Int -> Parser Test
parseHigherLevel i = do
  (j, titl) <- parseLevelDecl i
  comment
  code <- parseCode
  comment
  next <- many (parseHigherLevel (j + 1)) <|> (return <$> parseExpected)
  comment
  return (BaseCode titl code next)

parseAnyLevel :: Parser Test
parseAnyLevel = parseHigherLevel 0

parseModule ::  FilePath -> L.Text -> Either ParseError [Test]
parseModule = parse (sepBy parseAnyLevel (string "\n"))
