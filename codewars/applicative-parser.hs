module ApplicativeParser where

import Data.Char
import Data.List (isPrefixOf)
import Prelude   hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P $ \s -> mapSnd f `map` unP p s

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) a = pmap $ const a

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P $ \s ->
    case s of
        c : cs | p c -> [(cs, c)]
        _      -> []

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP c = predP (== c)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject a = P $ \s -> [(s, a)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P $ \s -> do
    (s', f) <- unP pf s
    (s'', a) <- unP px s'
    return (s'', f a)

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = P $ \s -> do
    (s', a) <- unP pa s
    (s'', _) <- unP pb s'
    return (s'', a)

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = P $ \s -> do
    (s', _) <- unP pa s
    unP pb s'

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP p = P $ \s -> [(drop (length p) s, p) | p `isPrefixOf` s]

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ const []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
a1 <<>> a2 = P $ \s -> unP a1 s ++ unP a2 s

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = some p <<>> inject []

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = P $ \s ->
    let single = unP p s
     in (mapSnd return <$> single) ++ do
        (s', a) <- single
        (s'', as) <- unP (some p) s'
        return (s'', a : as)


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = snd <$> (null . fst) `filter` unP p cs

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = case runParser p cs of
    [a] -> Just a
    _   -> Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

fromChar :: Char -> BinOp
fromChar '+' = AddBO
fromChar '*' = MulBO
fromChar _   = error "impossible"

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
            | BinOpE BinOp Expr Expr
            | NegE Expr
            | ZeroE
            deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE i) = i
evalExpr (BinOpE op lhs rhs) =
    let o = if op == AddBO then (+) else (*)
     in evalExpr lhs `o` evalExpr rhs
evalExpr (NegE e) = - evalExpr e
evalExpr ZeroE = 0

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
--

space :: Parser Char
space = charP ' '

between :: Parser open -> Parser mid -> Parser close -> Parser mid
between po pm pc = po @> pm <@ pc

digit :: Parser Char
digit = predP isDigit

int :: Parser Expr
int = ConstE . read <#> many digit

zero :: Parser Expr
zero = ZeroE <# charP 'z'

binOp :: Parser BinOp
binOp = fromChar <#> (charP '+' <<>> charP '*')

binOpExpr :: Parser Expr
binOpExpr = inject (flip BinOpE) <@>
    (charP '(' @> expr <@ space) <@> binOp <@> (space @> expr <@ charP ')')

neg :: Parser Expr
neg = NegE <#> (charP '-' @> expr)

expr :: Parser Expr
expr = int <<>> zero <<>> binOpExpr <<>> neg

parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique expr
