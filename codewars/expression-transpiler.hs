module Transpiler where

import Data.Char
import Data.List
import Text.ParserCombinators.ReadP
import Text.Printf

newtype NameOrNumber = NN String deriving (Show)

data Lambda = Lambda [NameOrNumber] [NameOrNumber] deriving (Show)

data Expression = Id String | L Lambda deriving (Show)

data Call = Call Expression [Expression] deriving (Show)

data Source = Func Call | Expr Expression deriving (Show)

space :: ReadP ()
space = () <$ satisfy (`elem` "\n ")

spaces :: ReadP ()
spaces = () <$ many space

identifier :: ReadP String
identifier = do
    h <- satisfy $ \c -> isAlpha c || c == '_'
    t <- munch $ \c -> isAlphaNum c || c == '_'
    return $ h : t

number :: ReadP String
number = munch1 isDigit

nameOrNumber :: ReadP String
nameOrNumber = identifier +++ number

symbolC :: Char -> ReadP Char
symbolC c = between spaces spaces (char c)

symbol :: ReadP a -> ReadP a
symbol = between spaces spaces

lambda :: ReadP Lambda
lambda = do
    symbolC '{'
    params <- option [] paramLists
    spaces
    body <- option [] bodyLists
    symbolC '}'
    return $ Lambda params body
    where paramLists = do
            names <- nameOrNumber `sepBy1` symbolC ','
            symbol $ string "->"
            return $ NN <$> names
          bodyLists = do
            names <- nameOrNumber `sepBy1` many1 space
            return $ NN <$> names

expression :: ReadP Expression
expression = (Id <$> nameOrNumber) +++ (L <$> lambda)

source :: ReadP Source
source = (Expr <$> expression) +++ (Func <$> call)

call :: ReadP Call
call = do
    func <- expression
    spaces
    arguments <- (return . L <$> lambda) +++ argList
    return $ Call func arguments
    where argList = do
            exprs <- between (symbolC '(') (symbolC ')')
                (expression `sepBy` symbolC ',')
            spaces
            l <- option [] (return . L <$> lambda)
            return $ exprs ++ l

parseSource :: String -> Either String Source
parseSource s = case (null . snd) `filter` readP_to_S source s of
    (x, _) : _ -> return x
    _          -> Left "Hugh?"

toTarget :: Source -> String
toTarget (Expr (Id s))                    = s
toTarget (Expr (L (Lambda params stmts))) =
    printf "(%s){%s}" (intercalate "," $ printName <$> params)
                      (intercalate "" $ (++";") . printName <$> stmts)
    where printName (NN name) = name
toTarget (Func (Call expr exprs))         =
    printf "%s(%s)" (targetExpr expr) (intercalate "," $ targetExpr <$> exprs)
    where targetExpr = toTarget . Expr

transpile :: String -> Either String String
transpile s = toTarget <$> parseSource s
