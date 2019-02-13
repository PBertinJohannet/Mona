import Text.ParserCombinators.Parsec
import Text.Parsec

data IntExp = Const Int
            | Add IntExp IntExp
            | Sub IntExp IntExp deriving Show

parseConstant :: Parser IntExp
parseConstant =
  do xs <- many1 digit
     return $ Const (read xs :: Int)

parseOperation :: Parser (IntExp -> IntExp -> IntExp)
parseOperation =
 do spaces
    symbol <- char '+' <|> char '-'
    spaces
    case symbol of
      '+' -> return Add
      '-' -> return Sub

parseIntExp :: Parser IntExp
parseIntExp =
  chainl1 parseConstant parseOperation

main = print $ parse parseIntExp "ok" "1+2+3"
