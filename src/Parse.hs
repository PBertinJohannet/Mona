{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- This code was auto-generated and might be rewritten
module Parse where
import Text.ParserCombinators.Parsec
import Text.Parsec hiding (try)
import Data.Functor.Identity
import Control.Applicative hiding (many, (<|>))
import Text.Parsec.Pos

data Token = TokenRelOperator RelOperator | TokenAddOperator AddOperator | TokenMulOperator MulOperator | TokenName Name | TokenStringLiteral StringLiteral | TokenIfOperator IfOperator | TokenNumtoken Numtoken | Tokenarrow  | Tokensemicolon  | Tokencomma  | Tokencolon  | Tokenlambda  | Tokenlet  | TokenopenPar  | TokenclosePar  | Tokenfix  | Tokenin  | Tokenequal  deriving Show
data IfOperator = IfOperatorifToken  | IfOperatorwhile  | IfOperatorthen  | IfOperatorelse  deriving Show
data RelOperator = RelOperatorlt  | RelOperatorlte  | RelOperatoreq  | RelOperatorneq  | RelOperatorgte  | RelOperatorgt  deriving Show
data AddOperator = AddOperatorplus  | AddOperatorminus  deriving Show
data MulOperator = MulOperatortimes  | MulOperatordiv  | MulOperatormod  | MulOperatorlshift  | MulOperatorlshift3  | MulOperatorrshift  deriving Show
type StringLiteral = ( String )
type Name = [AlphaOnce]
type AlphaNum = [AlphaNumOnce]
type AlphaNumOnce = AlphaOnce
type AlphaOnce = Char
type Numtoken = Char

isarrow :: Token -> Bool
isarrow Tokenarrow = True
isarrow _ = False

issemicolon :: Token -> Bool
issemicolon Tokensemicolon = True
issemicolon _ = False

iscomma :: Token -> Bool
iscomma Tokencomma = True
iscomma _ = False

iscolon :: Token -> Bool
iscolon Tokencolon = True
iscolon _ = False

islambda :: Token -> Bool
islambda Tokenlambda = True
islambda _ = False

islet :: Token -> Bool
islet Tokenlet = True
islet _ = False

isopenPar :: Token -> Bool
isopenPar TokenopenPar = True
isopenPar _ = False

isclosePar :: Token -> Bool
isclosePar TokenclosePar = True
isclosePar _ = False

isfix :: Token -> Bool
isfix Tokenfix = True
isfix _ = False

isin :: Token -> Bool
isin Tokenin = True
isin _ = False

isequal :: Token -> Bool
isequal Tokenequal = True
isequal _ = False

isToken :: Token -> Bool
isToken _ = False

isifToken :: Token -> Bool
isifToken (TokenIfOperator (IfOperatorifToken)) = True
isifToken _ = False

iswhile :: Token -> Bool
iswhile (TokenIfOperator (IfOperatorwhile)) = True
iswhile _ = False

isthen :: Token -> Bool
isthen (TokenIfOperator (IfOperatorthen)) = True
isthen _ = False

iselse :: Token -> Bool
iselse (TokenIfOperator (IfOperatorelse)) = True
iselse _ = False

isIfOperator :: Token -> Bool
isIfOperator (TokenIfOperator a) = True
isIfOperator _ = False

islt :: Token -> Bool
islt (TokenRelOperator (RelOperatorlt)) = True
islt _ = False

islte :: Token -> Bool
islte (TokenRelOperator (RelOperatorlte)) = True
islte _ = False

iseq :: Token -> Bool
iseq (TokenRelOperator (RelOperatoreq)) = True
iseq _ = False

isneq :: Token -> Bool
isneq (TokenRelOperator (RelOperatorneq)) = True
isneq _ = False

isgte :: Token -> Bool
isgte (TokenRelOperator (RelOperatorgte)) = True
isgte _ = False

isgt :: Token -> Bool
isgt (TokenRelOperator (RelOperatorgt)) = True
isgt _ = False

isRelOperator :: Token -> Bool
isRelOperator (TokenRelOperator a) = True
isRelOperator _ = False

isplus :: Token -> Bool
isplus (TokenAddOperator (AddOperatorplus)) = True
isplus _ = False

isminus :: Token -> Bool
isminus (TokenAddOperator (AddOperatorminus)) = True
isminus _ = False

isAddOperator :: Token -> Bool
isAddOperator (TokenAddOperator a) = True
isAddOperator _ = False

istimes :: Token -> Bool
istimes (TokenMulOperator (MulOperatortimes)) = True
istimes _ = False

isdiv :: Token -> Bool
isdiv (TokenMulOperator (MulOperatordiv)) = True
isdiv _ = False

ismod :: Token -> Bool
ismod (TokenMulOperator (MulOperatormod)) = True
ismod _ = False

islshift :: Token -> Bool
islshift (TokenMulOperator (MulOperatorlshift)) = True
islshift _ = False

islshift3 :: Token -> Bool
islshift3 (TokenMulOperator (MulOperatorlshift3)) = True
islshift3 _ = False

isrshift :: Token -> Bool
isrshift (TokenMulOperator (MulOperatorrshift)) = True
isrshift _ = False

isMulOperator :: Token -> Bool
isMulOperator (TokenMulOperator a) = True
isMulOperator _ = False

isStringLiteral :: Token -> Bool
isStringLiteral (TokenStringLiteral a) = True
isStringLiteral _ = False

isName :: Token -> Bool
isName (TokenName a) = True
isName _ = False

isAlphaNum :: Token -> Bool
isAlphaNum _ = False

isAlphaNumOnce :: Token -> Bool
isAlphaNumOnce _ = False

isAlphaOnce :: Token -> Bool
isAlphaOnce _ = False

isNumtoken :: Token -> Bool
isNumtoken (TokenNumtoken a) = True
isNumtoken _ = False



asIfOperator :: Token -> IfOperator
asIfOperator (TokenIfOperator a) = a

asRelOperator :: Token -> RelOperator
asRelOperator (TokenRelOperator a) = a

asAddOperator :: Token -> AddOperator
asAddOperator (TokenAddOperator a) = a

asMulOperator :: Token -> MulOperator
asMulOperator (TokenMulOperator a) = a

asStringLiteral :: Token -> StringLiteral
asStringLiteral (TokenStringLiteral a) = a

asName :: Token -> Name
asName (TokenName a) = a

asNumtoken :: Token -> Numtoken
asNumtoken (TokenNumtoken a) = a



parseToken :: GenParser Char () [(Token, SourcePos)]
parseToken = many (do {
    p <- getPosition;
    t <- try (TokenRelOperator<$>(parseRelOperator)) <|> try (TokenAddOperator<$>(parseAddOperator)) <|> try (TokenMulOperator<$>(parseMulOperator)) <|> try (TokenName<$>(parseName)) <|> try (TokenStringLiteral<$>(parseStringLiteral)) <|> try (TokenIfOperator<$>(parseIfOperator)) <|> try (TokenNumtoken<$>(parseNumtoken)) <|> try (Tokenarrow <$ (string "->")) <|> try (Tokensemicolon <$ (string ";")) <|> try (Tokencomma <$ (string ",")) <|> try (Tokencolon <$ (string ":")) <|> try (Tokenlambda <$ (string "\\")) <|> try (Tokenlet <$ (string "let")) <|> try (TokenopenPar <$ (string "openPar")) <|> try (TokenclosePar <$ (string "closePar")) <|> try (Tokenfix <$ (string "fix")) <|> try (Tokenin <$ (string "in")) <|> (Tokenequal <$ (string "="));
    skipBlank;
    return (t, p)})

parseIfOperator :: GenParser Char () IfOperator
parseIfOperator = try (IfOperatorifToken <$ (string "if")) <|> try (IfOperatorwhile <$ (string "while")) <|> try (IfOperatorthen <$ (string "then")) <|> (IfOperatorelse <$ (string "else"))

parseRelOperator :: GenParser Char () RelOperator
parseRelOperator = try (RelOperatorlt <$ (string "<")) <|> try (RelOperatorlte <$ (string "<=")) <|> try (RelOperatoreq <$ (string "==")) <|> try (RelOperatorneq <$ (string "!=")) <|> try (RelOperatorgte <$ (string ">=")) <|> (RelOperatorgt <$ (string ">"))

parseAddOperator :: GenParser Char () AddOperator
parseAddOperator = try (AddOperatorplus <$ (string "+")) <|> (AddOperatorminus <$ (string "-"))

parseMulOperator :: GenParser Char () MulOperator
parseMulOperator = try (MulOperatortimes <$ (string "*")) <|> try (MulOperatordiv <$ (string "/")) <|> try (MulOperatormod <$ (string "%")) <|> try (MulOperatorlshift <$ (string "<<")) <|> try (MulOperatorlshift3 <$ (string "<<<")) <|> (MulOperatorrshift <$ (string ">>"))

parseStringLiteral :: GenParser Char () StringLiteral
parseStringLiteral = ((do {
    (string "\"");
    one <- (manyTill anyChar ( lookAhead (string "\"")));
    (string "\"");
    return (one)}))

parseName :: GenParser Char () Name
parseName = ((liftA2 (:) ((parseAlphaOnce)) ((parseAlphaNum))))

parseAlphaNum :: GenParser Char () AlphaNum
parseAlphaNum = ((many (parseAlphaNumOnce)) )

parseAlphaNumOnce :: GenParser Char () AlphaNumOnce
parseAlphaNumOnce = ((parseAlphaOnce) <|> (digit))

parseAlphaOnce :: GenParser Char () AlphaOnce
parseAlphaOnce = ((oneOf ['A'..'Z']) <|> (oneOf ['a'..'z']))

parseNumtoken :: GenParser Char () Numtoken
parseNumtoken = ((digit))

type Root = (Statement)
data Statement = Statement (Name, Expr) deriving Show
data Expr = ExprParenExpr ParenExpr | ExprApp App | ExprOpExpr OpExpr | ExprFix Fix | ExprLetIn LetIn | ExprLamExpr LamExpr deriving Show
data ParenExpr = ParenExpr (Expr) deriving Show
data App = App (Expr, Expr) deriving Show
data OpExpr = OpExpr (T0, [(RelOperator, T0)]) deriving Show
data T0 = T0 (T1, [(AddOperator, T1)]) deriving Show
data T1 = T1 (Item, [(MulOperator, Item)]) deriving Show
data Item = ItemNumtoken Numtoken | ItemStringLiteral StringLiteral | ItemName Name deriving Show
data Fix = Fix (Expr) deriving Show
data LetIn = LetIn (Name, Expr, Expr) deriving Show
data LamExpr = LamExpr ([Name], Expr) deriving Show

isRoot :: Root -> Bool
isRoot _ = False

isStatement :: Root -> Bool
isStatement _ = False

isExpr :: Root -> Bool
isExpr _ = False

isParenExpr :: Root -> Bool
isParenExpr _ = False

isApp :: Root -> Bool
isApp _ = False

isOpExpr :: Root -> Bool
isOpExpr _ = False

isT0 :: Root -> Bool
isT0 _ = False

isT1 :: Root -> Bool
isT1 _ = False

isItem :: Root -> Bool
isItem _ = False

isFix :: Root -> Bool
isFix _ = False

isLetIn :: Root -> Bool
isLetIn _ = False

isLamExpr :: Root -> Bool
isLamExpr _ = False





parseRoot :: GenParser (Token, SourcePos) () [(Root, SourcePos)]
parseRoot = many (do {
    p <- getPosition;
    t <- ((do {
    zero <- (parseStatement);
    (optionMaybe ( ( satisfyT issemicolon)));
    return (zero)}));
    return (t, p)})

parseStatement :: GenParser (Token, SourcePos) () Statement
parseStatement = Statement <$> ((do {
    ( ( satisfyT islet));
    one <- (asName <$> ( satisfyT isName));
    ( ( satisfyT isequal));
    three <- (parseExpr);
    return (one, three)}))

parseExpr :: GenParser (Token, SourcePos) () Expr
parseExpr = try (ExprParenExpr<$>(parseParenExpr)) <|> try (ExprApp<$>(parseApp)) <|> try (ExprOpExpr<$>(parseOpExpr)) <|> try (ExprFix<$>(parseFix)) <|> try (ExprLetIn<$>(parseLetIn)) <|> (ExprLamExpr<$>(parseLamExpr))

parseParenExpr :: GenParser (Token, SourcePos) () ParenExpr
parseParenExpr = ParenExpr <$> ((do {
    ( ( satisfyT isopenPar));
    one <- (parseExpr);
    ( ( satisfyT isclosePar));
    return (one)}))

parseApp :: GenParser (Token, SourcePos) () App
parseApp = App <$> ((do {
    zero <- (parseExpr);
    one <- (parseExpr);
    return (zero, one)}))

parseOpExpr :: GenParser (Token, SourcePos) () OpExpr
parseOpExpr = OpExpr <$> ((do {
    zero <- (parseT0);
    one <- (many (do {
    zero <- (asRelOperator <$> ( satisfyT isRelOperator));
    one <- (parseT0);
    return (zero, one)})) ;
    return (zero, one)}))

parseT0 :: GenParser (Token, SourcePos) () T0
parseT0 = T0 <$> ((do {
    zero <- (parseT1);
    one <- (many (do {
    zero <- (asAddOperator <$> ( satisfyT isAddOperator));
    one <- (parseT1);
    return (zero, one)})) ;
    return (zero, one)}))

parseT1 :: GenParser (Token, SourcePos) () T1
parseT1 = T1 <$> ((do {
    zero <- (parseItem);
    one <- (many (do {
    zero <- (asMulOperator <$> ( satisfyT isMulOperator));
    one <- (parseItem);
    return (zero, one)})) ;
    return (zero, one)}))

parseItem :: GenParser (Token, SourcePos) () Item
parseItem = try (ItemNumtoken<$>(asNumtoken <$> ( satisfyT isNumtoken))) <|> try (ItemStringLiteral<$>(asStringLiteral <$> ( satisfyT isStringLiteral))) <|> (ItemName<$>(asName <$> ( satisfyT isName)))

parseFix :: GenParser (Token, SourcePos) () Fix
parseFix = Fix <$> ((do {
    ( ( satisfyT isfix));
    one <- (parseExpr);
    return (one)}))

parseLetIn :: GenParser (Token, SourcePos) () LetIn
parseLetIn = LetIn <$> ((do {
    ( ( satisfyT islet));
    one <- (asName <$> ( satisfyT isName));
    ( ( satisfyT isequal));
    three <- (parseExpr);
    ( ( satisfyT isin));
    five <- (parseExpr);
    return (one, three, five)}))

parseLamExpr :: GenParser (Token, SourcePos) () LamExpr
parseLamExpr = LamExpr <$> ((do {
    ( ( satisfyT islambda));
    one <- (many (asName <$> ( satisfyT isName))) ;
    ( ( satisfyT isarrow));
    three <- (parseExpr);
    return (one, three)}))



skipBlank :: GenParser Char () ()
skipBlank = skipMany (oneOf " \n\t")

satisfyT :: (Show a) => (a -> Bool) -> ParsecT [(a, SourcePos)] () Identity a
satisfyT p = tokenPrim showTok advance testTok
    where
      showTok t     = show t
      testTok (t, _) = if p t then Just t else Nothing

advance _ _ ((_, pos) : _) = pos
advance pos _ [] = pos

parseAll :: String -> Either ParseError [(Root, SourcePos)]
parseAll s = (parse (parseToken <* eof) "Token" $ s)>>= (parse (parseRoot <* eof) "Root")

parseOne :: String -> Either ParseError Root
parseOne s = fst <$> head <$> (parseAll s)
