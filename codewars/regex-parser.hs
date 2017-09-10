module RegExpParser (RegExp(..), parseRegExp) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Maybe

data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any charater
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp
            | Or RegExp RegExp  -- ^ A choice between 2 regexps
            | Str [RegExp]      -- ^ A sequence of regexps.
            deriving (Show, Eq)

data PS = PS String (Maybe RegExp) Bool

modifyInput :: (String -> String) -> Parser ()
modifyInput f = do
    PS i l b <- get
    put $ PS (f i) l b

modifyLast :: (Maybe RegExp -> Maybe RegExp) -> Parser ()
modifyLast f = do
    PS i l b <- get
    put $ PS i (f l) b

pack :: Parser ()
pack = do
    PS a b _ <- get
    put $ PS a b True

unpack :: Parser ()
unpack = do
    PS a b _ <- get
    put $ PS a b False

packed :: Parser Bool
packed = do
    PS _ _ p <- get
    return p

putInput :: String -> Parser ()
putInput = modifyInput . const

putLast :: Maybe RegExp -> Parser ()
putLast = modifyLast . const

type Parser = StateT PS Maybe

normalChar :: Char -> Bool
normalChar = not . flip elem "()*.|"

charPred :: (Char -> Bool) -> Parser Char
charPred p = do
    PS cs _ _ <- get
    case cs of
        c : rest | p c -> putInput rest >> return c
        _        -> lift Nothing

char :: Char -> Parser Char
char c = charPred (== c)

normal :: Parser RegExp
normal = Normal <$> charPred normalChar

anyExp :: Parser RegExp
anyExp = Any <$ char '.'

paren :: Parser RegExp
paren = do
    PS _ l _ <- get
    putLast Nothing
    char '('
    r <- basic
    char ')'
    putLast l
    when (isNothing l) pack
    return r

star :: Parser ()
star = do
    char '*'
    PS _ l p <- get
    case l of
        Nothing -> lift Nothing
        Just (ZeroOrMore _) -> lift Nothing
        Just regex | p -> putLast $ Just (ZeroOrMore regex)
        Just (Str ls)  -> case last ls of
            ZeroOrMore _ -> lift Nothing
            other        -> putLast $ Just (Str $ init ls ++ [ZeroOrMore other])
        Just single -> putLast $ Just $ ZeroOrMore single
    unpack

orExp :: Parser RegExp
orExp = do
    char '|'
    PS _ l _ <- get
    case l of
        Just alt -> do
            putLast Nothing
            alt2 <- basic
            return $ Or alt alt2
        _ -> lift Nothing

push :: RegExp -> Parser ()
push regex = do
    PS _ l p <- get
    case l of
        Nothing       -> putLast $ Just regex
        Just r        | p -> putLast $ Just (Str [r, regex])
        Just (Str rs) -> unpack >> putLast (Just (Str $ rs ++ [regex]))
        Just r        -> unpack >> putLast (Just (Str [r, regex]))

basic :: Parser RegExp
basic = do
    PS i l _ <- get
    case (i, l) of
        ([], Nothing) -> lift Nothing
        ([], Just regex) -> do
            putLast Nothing
            return regex
        ('|' : _, _) -> do
            r <- orExp
            putLast $ Just r
            basic
        ('(' : _, _) -> do
            r <- paren
            push r
            basic
        (')' : _, Just regex) -> do
            p <- packed
            return regex
        (')' : _, Nothing) -> lift Nothing
        ('.' : _, _) -> do
            r <- anyExp
            push r
            basic
        ('*' : _, _) -> star >> basic
        (_ : _, _)   -> do
            r <- normal
            push r
            basic

runParser :: Parser a -> String -> Maybe a
runParser p s = case runStateT p (PS s Nothing False) of
    Just (a, PS "" _ _) -> Just a
    _                   -> Nothing

parseRegExp :: String -> Maybe RegExp
parseRegExp = runParser basic
