{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- This code was auto-generated and might be rewritten
module Grammar where
import Text.ParserCombinators.Parsec
import Text.Parsec hiding (try)
import Data.Functor.Identity
import Control.Applicative hiding (many, (<|>))
import Text.Parsec.Pos

import System.IO
import System.Environment
data Token = TokenKeyword Keyword | TokenRelOperator RelOperator | TokenAddOperator AddOperator | TokenMulOperator MulOperator | TokenName Name | TokenStringLiteral StringLiteral | TokenAssignOperator AssignOperator | TokenNumtoken Numtoken | Tokensemicolon  | Tokencomma  | Tokencolon  deriving Show
data Keyword = KeywordifToken  | Keywordwhile  | Keywordprint  | Keywordread  | KeyworddoToken  | Keyworduntil  | Keywordend  | KeywordthenToken  | Keywordelif  | Keywordbreak  | Keywordhalt  | Keywordcontinue  | KeywordelseToken  deriving Show
data AssignOperator = AssignOperatorequal  | AssignOperatorinc  | AssignOperatordec  deriving Show
data RelOperator = RelOperatorlt  | RelOperatorlte  | RelOperatoreq  | RelOperatorneq  | RelOperatorgte  | RelOperatorgt  deriving Show
data AddOperator = AddOperatorplus  | AddOperatorminus  deriving Show
data MulOperator = MulOperatortimes  | MulOperatordiv  | MulOperatormod  | MulOperatorlshift  | MulOperatorlshift3  | MulOperatorrshift  deriving Show
type StringLiteral = ( String )
type Name = [AlphaOnce]
type AlphaNum = [AlphaNumOnce]
type AlphaNumOnce = AlphaOnce
type AlphaOnce = Char
type Numtoken = Char

issemicolon :: Token -> Bool
issemicolon Tokensemicolon = True
issemicolon _ = False

iscomma :: Token -> Bool
iscomma Tokencomma = True
iscomma _ = False

iscolon :: Token -> Bool
iscolon Tokencolon = True
iscolon _ = False

isToken :: Token -> Bool
isToken _ = False

isifToken :: Token -> Bool
isifToken (TokenKeyword (KeywordifToken)) = True
isifToken _ = False

iswhile :: Token -> Bool
iswhile (TokenKeyword (Keywordwhile)) = True
iswhile _ = False

isprint :: Token -> Bool
isprint (TokenKeyword (Keywordprint)) = True
isprint _ = False

isread :: Token -> Bool
isread (TokenKeyword (Keywordread)) = True
isread _ = False

isdoToken :: Token -> Bool
isdoToken (TokenKeyword (KeyworddoToken)) = True
isdoToken _ = False

isuntil :: Token -> Bool
isuntil (TokenKeyword (Keyworduntil)) = True
isuntil _ = False

isend :: Token -> Bool
isend (TokenKeyword (Keywordend)) = True
isend _ = False

isthenToken :: Token -> Bool
isthenToken (TokenKeyword (KeywordthenToken)) = True
isthenToken _ = False

iselif :: Token -> Bool
iselif (TokenKeyword (Keywordelif)) = True
iselif _ = False

isbreak :: Token -> Bool
isbreak (TokenKeyword (Keywordbreak)) = True
isbreak _ = False

ishalt :: Token -> Bool
ishalt (TokenKeyword (Keywordhalt)) = True
ishalt _ = False

iscontinue :: Token -> Bool
iscontinue (TokenKeyword (Keywordcontinue)) = True
iscontinue _ = False

iselseToken :: Token -> Bool
iselseToken (TokenKeyword (KeywordelseToken)) = True
iselseToken _ = False

isKeyword :: Token -> Bool
isKeyword (TokenKeyword a) = True
isKeyword _ = False

isequal :: Token -> Bool
isequal (TokenAssignOperator (AssignOperatorequal)) = True
isequal _ = False

isinc :: Token -> Bool
isinc (TokenAssignOperator (AssignOperatorinc)) = True
isinc _ = False

isdec :: Token -> Bool
isdec (TokenAssignOperator (AssignOperatordec)) = True
isdec _ = False

isAssignOperator :: Token -> Bool
isAssignOperator (TokenAssignOperator a) = True
isAssignOperator _ = False

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



asKeyword :: Token -> Keyword
asKeyword (TokenKeyword a) = a

asAssignOperator :: Token -> AssignOperator
asAssignOperator (TokenAssignOperator a) = a

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
    t <- try (TokenKeyword<$>(parseKeyword)) <|> try (TokenRelOperator<$>(parseRelOperator)) <|> try (TokenAddOperator<$>(parseAddOperator)) <|> try (TokenMulOperator<$>(parseMulOperator)) <|> try (TokenName<$>(parseName)) <|> try (TokenStringLiteral<$>(parseStringLiteral)) <|> try (TokenAssignOperator<$>(parseAssignOperator)) <|> try (TokenNumtoken<$>(parseNumtoken)) <|> try (Tokensemicolon <$ (string ";")) <|> try (Tokencomma <$ (string ",")) <|> (Tokencolon <$ (string ":"));
    skipBlank;
    return (t, p)})

parseKeyword :: GenParser Char () Keyword
parseKeyword = try (KeywordifToken <$ (string "if")) <|> try (Keywordwhile <$ (string "while")) <|> try (Keywordprint <$ (string "print")) <|> try (Keywordread <$ (string "input")) <|> try (KeyworddoToken <$ (string "do")) <|> try (Keyworduntil <$ (string "until")) <|> try (Keywordend <$ (string "end")) <|> try (KeywordthenToken <$ (string "then")) <|> try (Keywordelif <$ (string "elif")) <|> try (Keywordbreak <$ (string "break")) <|> try (Keywordhalt <$ (string "halt")) <|> try (Keywordcontinue <$ (string "continue")) <|> (KeywordelseToken <$ (string "else"))

parseAssignOperator :: GenParser Char () AssignOperator
parseAssignOperator = try (AssignOperatorequal <$ (string "=")) <|> try (AssignOperatorinc <$ (string "++")) <|> (AssignOperatordec <$ (string "--"))

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
type StatementList = [(Statement)]
data Statement = StatementIfStatement IfStatement | StatementAssignment Assignment | StatementWhileStatement WhileStatement | StatementPrint Print | StatementInput Input | StatementUntilStatement UntilStatement deriving Show
data WhileStatement = WhileStatement (Expression, StatementList) deriving Show
data UntilStatement = UntilStatement (Expression, StatementList) deriving Show
data Do = Do (StatementList, DoEnd) deriving Show
data DoEnd = DoEndZero  (Expression) | DoEndOne  (Expression) deriving Show
data IfStatement = IfStatement (Expression, StatementList, [(Expression, StatementList)],  Maybe (StatementList)) deriving Show
data Print = Print (Item, [(Item)]) deriving Show
data Input = Input (Name, [(Name)]) deriving Show
data Assignment = Assignment (Name, AssignOperator, Expression) deriving Show
data Expression = Expression (T0, [(RelOperator, T0)]) deriving Show
data T0 = T0 (T1, [(AddOperator, T1)]) deriving Show
data T1 = T1 (Item, [(MulOperator, Item)]) deriving Show
data Item = ItemNumtoken Numtoken | ItemStringLiteral StringLiteral | ItemName Name deriving Show

isRoot :: Root -> Bool
isRoot _ = False

isStatementList :: Root -> Bool
isStatementList _ = False

isStatement :: Root -> Bool
isStatement _ = False

isWhileStatement :: Root -> Bool
isWhileStatement _ = False

isUntilStatement :: Root -> Bool
isUntilStatement _ = False

isDo :: Root -> Bool
isDo _ = False

isDoEnd :: Root -> Bool
isDoEnd _ = False

isIfStatement :: Root -> Bool
isIfStatement _ = False

isPrint :: Root -> Bool
isPrint _ = False

isInput :: Root -> Bool
isInput _ = False

isAssignment :: Root -> Bool
isAssignment _ = False

isExpression :: Root -> Bool
isExpression _ = False

isT0 :: Root -> Bool
isT0 _ = False

isT1 :: Root -> Bool
isT1 _ = False

isItem :: Root -> Bool
isItem _ = False





parseRoot :: GenParser (Token, SourcePos) () [(Root, SourcePos)]
parseRoot = many (do {
    p <- getPosition;
    t <- ((do {
    zero <- (parseStatement);
    (optionMaybe ( ( satisfyT issemicolon)));
    return (zero)}));
    return (t, p)})

parseStatementList :: GenParser (Token, SourcePos) () StatementList
parseStatementList = ((many (do {
    zero <- (parseStatement);
    (optionMaybe ( ( satisfyT issemicolon)));
    return (zero)})) )

parseStatement :: GenParser (Token, SourcePos) () Statement
parseStatement = try (StatementIfStatement<$>(parseIfStatement)) <|> try (StatementAssignment<$>(parseAssignment)) <|> try (StatementWhileStatement<$>(parseWhileStatement)) <|> try (StatementPrint<$>(parsePrint)) <|> try (StatementInput<$>(parseInput)) <|> (StatementUntilStatement<$>(parseUntilStatement))

parseWhileStatement :: GenParser (Token, SourcePos) () WhileStatement
parseWhileStatement = WhileStatement <$> ((do {
    ( ( satisfyT iswhile));
    one <- (parseExpression);
    ( ( satisfyT isdoToken));
    three <- (parseStatementList);
    ( ( satisfyT isend));
    return (one, three)}))

parseUntilStatement :: GenParser (Token, SourcePos) () UntilStatement
parseUntilStatement = UntilStatement <$> ((do {
    ( ( satisfyT isuntil));
    one <- (parseExpression);
    ( ( satisfyT isdoToken));
    three <- (parseStatementList);
    ( ( satisfyT isend));
    return (one, three)}))

parseDo :: GenParser (Token, SourcePos) () Do
parseDo = Do <$> ((do {
    ( ( satisfyT isdoToken));
    one <- (parseStatementList);
    two <- (parseDoEnd);
    ( ( satisfyT isend));
    return (one, two)}))

parseDoEnd :: GenParser (Token, SourcePos) () DoEnd
parseDoEnd = try (DoEndZero <$>(do {
    ( ( satisfyT isuntil));
    one <- (parseExpression);
    return (one)})) <|> (DoEndOne <$>(do {
    ( ( satisfyT iswhile));
    one <- (parseExpression);
    return (one)}))

parseIfStatement :: GenParser (Token, SourcePos) () IfStatement
parseIfStatement = IfStatement <$> ((do {
    ( ( satisfyT isifToken));
    one <- (parseExpression);
    ( ( satisfyT isthenToken));
    three <- (parseStatementList);
    four <- (many (do {
    ( ( satisfyT iselif));
    one <- (parseExpression);
    ( ( satisfyT isthenToken));
    three <- (parseStatementList);
    return (one, three)})) ;
    five <- (optionMaybe (do {
    ( ( satisfyT iselseToken));
    one <- (parseStatementList);
    return (one)}));
    ( ( satisfyT isend));
    return (one, three, four, five)}))

parsePrint :: GenParser (Token, SourcePos) () Print
parsePrint = Print <$> ((do {
    ( ( satisfyT isprint));
    one <- (parseItem);
    two <- (many (do {
    ( ( satisfyT iscomma));
    one <- (parseItem);
    return (one)})) ;
    return (one, two)}))

parseInput :: GenParser (Token, SourcePos) () Input
parseInput = Input <$> ((do {
    ( ( satisfyT isread));
    one <- (asName <$> ( satisfyT isName));
    two <- (many (do {
    ( ( satisfyT iscomma));
    one <- (asName <$> ( satisfyT isName));
    return (one)})) ;
    return (one, two)}))

parseAssignment :: GenParser (Token, SourcePos) () Assignment
parseAssignment = Assignment <$> ((do {
    zero <- (asName <$> ( satisfyT isName));
    one <- (asAssignOperator <$> ( satisfyT isAssignOperator));
    two <- (parseExpression);
    return (zero, one, two)}))

parseExpression :: GenParser (Token, SourcePos) () Expression
parseExpression = Expression <$> ((do {
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



--main :: IO ()
--main = ((show) <$> (fmap (genProduce traverseState)) <$> (fmap (map fst))  <$> parseAll <$> ((head <$> getArgs ) >>= readFile)) >>= putStrLn



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

data TraverseState = TraverseState {indent :: [Char]}
traverseState :: TraverseState
traverseState = TraverseState{indent = []}
withIndent t  e= t {indent= e }
getIndent TraverseState{indent= ret } = ret

class Translatable a where
      genProduce :: TraverseState -> a -> String
      genTraverse :: TraverseState -> [a] -> ([String], TraverseState)
      genTraverse t (a:as) = foldl (\a -> a `andThen` traversing)
                                (let (s, state) = traversing t a in ([s], state))
                                as
      genTraverse t [] = ([], t)
      traversing :: TraverseState -> a -> (String, TraverseState)
      traversing t a = (genProduce t a, t)

lmap :: (a, TraverseState) -> (a -> b) -> (b, TraverseState)
lmap (a, st) f = (f a, st)

rmap :: (a, TraverseState) -> (TraverseState -> TraverseState) -> (a, TraverseState)
rmap (a, st) f = (a, f st)

andThen :: Translatable a => ([String], TraverseState) -> (TraverseState -> a -> (String, TraverseState)) -> a -> ([String], TraverseState)
andThen (s, t) f expr = let (res, newT) = f t expr in (s++[res], newT)

getRes = fst

instance Translatable Char where
    genProduce _ c = [c]

instance Translatable String where
    genProduce _ s = s

instance Translatable a => Translatable (Maybe a) where
      genProduce t' (Just a) = genProduce t' a
      genProduce _ _ = ""
instance Translatable StatementList where
    genProduce t' ( (l )) = mconcat ( map (\(st) -> "\n" ++ (indent t') ++ (genProduce t' st)) l)

instance Translatable Statement where
    genProduce t' ( ( StatementIfStatement a)) = (genProduce t' a) ++ "\n"
    genProduce t' ((StatementAssignment a)) = (genProduce t' a) ++ "\n"
    genProduce t' ((StatementWhileStatement a)) = (genProduce t' a) ++ "\n"
    genProduce t' ((StatementPrint a)) = (genProduce t' a) ++ "\n"
    genProduce t' ((StatementInput a)) = (genProduce t' a) ++ "\n"
    genProduce t' ((StatementUntilStatement a)) = (genProduce t' a) ++ "\n"

instance Translatable WhileStatement where
    genProduce t' (WhileStatement (e, s )) = "while (" ++ (genProduce t' e) ++ "):" ++ let genProduce' x' = genProduce ( x'  `withIndent` ((indent t') ++ "\t")) in (genProduce' t' s)

instance Translatable UntilStatement where
    genProduce t' (UntilStatement (e, s )) = "while not (" ++ (genProduce t' e) ++ "):" ++ let genProduce' x' = genProduce ( x'  `withIndent` ((indent t') ++ "\t")) in (genProduce' t' s)

instance Translatable Do where
    genProduce t' (Do (st, e )) = (genProduce t' st) ++ "while (" ++ (genProduce t' e) ++ "):" ++ let genProduce' x' = genProduce ( x'  `withIndent` ((indent t') ++ "\t")) in (genProduce' t' st)

instance Translatable DoEnd where
    genProduce t' ( ( DoEndZero e)) = " not " ++ (genProduce t' e)
    genProduce t' ((DoEndOne e)) = (genProduce t' e)

instance Translatable IfStatement where
    genProduce t' (IfStatement (e, s1, elif, s3 )) = "if (" ++ (genProduce t' e) ++ "):" ++ let genProduce' x' = genProduce ( x'  `withIndent` ((indent t') ++ "\t")) in (genProduce' t' s1) ++ mconcat ( map (\(e2, s2) -> "elif (" ++ (genProduce t' e2) ++ "):" ++ let genProduce' x' = genProduce ( x'  `withIndent` ((indent t') ++ "\t")) in (genProduce' t' s2)) elif) ++ "else: " ++ let genProduce' x' = genProduce ( x'  `withIndent` ((indent t') ++ "\t")) in (genProduce' t' s3)

instance Translatable Print where
    genProduce t' (Print (i, p )) = "print (" ++ (genProduce t' i) ++ mconcat ( map (\(is) -> ", " ++ (genProduce t' is)) p) ++ ")"

instance Translatable Input where
    genProduce t' (Input (i, others )) = (genProduce t' i) ++ "= input()\n" ++ mconcat ( map (\(is) -> (genProduce t' is) ++ "= input()\n") others)

instance Translatable Assignment where
    genProduce t' (Assignment (n, a, e )) = (genProduce t' n) ++ (genProduce t' a) ++ (genProduce t' e)

instance Translatable AssignOperator where
    genProduce t' ( ( AssignOperatorequal )) = "="
    genProduce t' ((AssignOperatorinc )) = "++"
    genProduce t' ((AssignOperatordec )) = "-"

instance Translatable RelOperator where
    genProduce t' ( ( RelOperatorlt )) = "<"
    genProduce t' ((RelOperatorlte )) = "<="
    genProduce t' ((RelOperatoreq )) = "=="
    genProduce t' ((RelOperatorneq )) = "!="
    genProduce t' ((RelOperatorgte )) = ">="
    genProduce t' ((RelOperatorgt )) = ">"

instance Translatable AddOperator where
    genProduce t' ( ( AddOperatorplus )) = "+"
    genProduce t' ((AddOperatorminus )) = "-"

instance Translatable MulOperator where
    genProduce t' ( ( MulOperatortimes )) = "*"
    genProduce t' ((MulOperatordiv )) = "/"
    genProduce t' ((MulOperatormod )) = "%"
    genProduce t' ((MulOperatorlshift )) = "<<"
    genProduce t' ((MulOperatorlshift3 )) = "<<<"
    genProduce t' ((MulOperatorrshift )) = ">>"

instance Translatable Expression where
    genProduce t' (Expression (t, e )) = (genProduce t' t) ++ mconcat ( map (\(r, t) -> (genProduce t' r) ++ " " ++ (genProduce t' t)) e)

instance Translatable T0 where
    genProduce t' (T0 (t, e )) = (genProduce t' t) ++ mconcat ( map (\(r, t) -> (genProduce t' r) ++ " " ++ (genProduce t' t)) e)

instance Translatable T1 where
    genProduce t' (T1 (t, e )) = (genProduce t' t) ++ mconcat ( map (\(r, t) -> (genProduce t' r) ++ " " ++ (genProduce t' t)) e)

instance Translatable Item where
    genProduce t' ( ( ItemNumtoken n)) = (genProduce t' n)
    genProduce t' ((ItemStringLiteral s)) = "'" ++ (genProduce t' s) ++ "'"
    genProduce t' ((ItemName n)) = (genProduce t' n)
