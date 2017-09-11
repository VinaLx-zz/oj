module LispLovesMe where

import           Data.Char
import           Data.Foldable                (foldl')
import qualified Data.Map                     as M
import           Text.ParserCombinators.ReadP
import           Text.Printf

data AST = I32 Int
         | Sym String
         | Nul
         | Err
         | Lst [AST]
         | Boo Bool
         | Nod AST [AST]
         deriving (Eq, Show)

type LispFunc = [AST] -> AST

lAddMul :: (Int -> Int -> Int) -> Int -> LispFunc
lAddMul op i = foldl' step (I32 i) . (eval <$>)
    where step (I32 x) (I32 y) = I32 (x `op` y)
          step _ _             = Err

lMinDiv :: (Int -> Int -> Int) -> LispFunc
lMinDiv op (x : xs) = foldl' step (eval x) (eval <$> xs)
    where step (I32 x) (I32 y) = I32 (x `op` y)
          step _ _             = Err
lMinDiv _ [] = Err

lPower :: LispFunc
lPower [x, y] = case (eval x, eval y) of
    (I32 a, I32 b) -> I32 (a ^ b)
    _              -> Err
lPower _              = Err

lNot :: LispFunc
lNot [a] = case eval a of
    Boo b -> Boo (not b)
    _     -> Err
lNot _ = Err

lOrd :: (Int -> Int -> Bool) -> LispFunc
lOrd op [a, b] = case (eval a, eval b) of
    (I32 x, I32 y) -> Boo (x `op` y)
    _              -> Err
lOrd _ _ = Err

lEq :: LispFunc
lEq [a, b] = case (eval a, eval b) of
    (Boo x, Boo y) -> Boo (x == y)
    (I32 x, I32 y) -> Boo (x == y)
    _              -> Err
lEq _ = Err

lUneq :: LispFunc
lUneq [a, b] = case (eval a, eval b) of
    (Boo x, Boo y) -> Boo (x /= y)
    (I32 x, I32 y) -> Boo (x /= y)
    _              -> Err
lUneq _ = Err

lList :: LispFunc
lList asts =
    let tmp = eval <$> asts
     in if Err `elem` tmp then Err else Lst tmp

lSize :: LispFunc
lSize [ls] = case eval ls of
    Lst xs -> I32 (length xs)
    _      -> Err
lSize _        = Err

lReverse :: LispFunc
lReverse [ls] = case eval ls of
    Lst xs -> Lst (reverse xs)
    _      -> Err
lReverse _ = Err

lDots :: LispFunc
lDots [a, b] = case (eval a, eval b) of
    (I32 x, I32 y) -> Lst (I32 <$> [x .. y])
    _              -> Err
lDots _ = Err

lIf :: LispFunc
lIf [p, t, f] = case eval p of
    Boo True  -> eval t
    Boo False -> eval f
    _         -> Err
lIf [p, t] = case eval p of
    Boo True  -> eval t
    Boo False -> Nul
    _         -> Err
lIf _ = Err

preludeFunctions :: M.Map String LispFunc
preludeFunctions = M.fromList [
      ("+", lAddMul (+) 0)
    , ("*", lAddMul (*) 1)
    , ("-", lMinDiv (-))
    , ("/", lMinDiv div)
    , ("^", lPower)
    , (">", lOrd (>))
    , ("<", lOrd (<))
    , ("!", lNot)
    , ("list", lList)
    , ("size", lSize)
    , ("reverse", lReverse)
    , ("..", lDots)
    , ("==", lEq)
    , (">=", lOrd (>=))
    , ("<=", lOrd (<=))
    , ("!=", lUneq)
    , ("if", lIf)]

evalFunc :: AST -> [AST] -> AST
evalFunc ast args = case eval ast of
    Sym name -> case M.lookup name preludeFunctions of
        Just f -> f args
        _      -> Err
    _ -> Err

eval :: AST -> AST
eval (Nod ast asts) = eval (Lst $ ast : asts)
eval (Lst [])       = Nul
eval (Lst (h : t))  = evalFunc h t
eval other          = other

-- parser

space :: ReadP ()
space = () <$ satisfy (`elem` "\n\r\t, ")

spaces :: ReadP ()
spaces = () <$ many space

spaces1 :: ReadP ()
spaces1 = () <$ many1 space

i32 :: ReadP AST
i32 = I32 . read <$> many1 (satisfy isDigit)

nul :: ReadP AST
nul = Nul <$ string "null"

nulList :: ReadP AST
nulList = Nul <$ between (char '(') (char ')') spaces

boo :: ReadP AST
boo = (Boo True <$ string "true") +++ (Boo False <$ string "false")

symbol :: ReadP AST
symbol = do
    h <- satisfy $ \c -> c `notElem` "() \t\r\n," && not (isDigit c)
    rest <- munch $ \c -> c `notElem` "() \n\t\r,"
    return $ Sym (h : rest)

list :: ReadP AST
list = between (char '(') (char ')') $ do
    spaces
    ls <- itemList
    spaces
    return (Lst ls)
    where itemList = do
            h <- allTok
            t <- many item
            return (h : t)
          item = (spaces >> (nulList <++ list)) +++
                 (spaces1 >> (i32 +++ boo +++ nul <++ symbol))

allTok :: ReadP AST
allTok = i32 +++ (nulList <++ list) +++ (boo +++ nul <++ symbol)

lisp :: ReadP AST
lisp = between spaces spaces allTok

runParser :: String -> Maybe AST
runParser s = case (null . snd) `filter` readP_to_S lisp s of
    (ast, _) : _ -> Just ast
    _            -> Nothing

-- top level

prettyPrint :: AST -> String
prettyPrint (I32 i)     = show i
prettyPrint (Boo True)  = "true"
prettyPrint (Boo False) = "false"
prettyPrint Nul         = "null"
prettyPrint (Nod h t)   = prettyPrint (Lst (h : t))
prettyPrint Err         = "error"
prettyPrint (Lst ls)    = printf "(%s)" (unwords $ prettyPrint <$> ls)
prettyPrint (Sym s)     = s

lispPretty :: String -> Maybe String
lispPretty s = prettyPrint <$> runParser s

lispEval :: String -> Maybe AST
lispEval s = eval <$> runParser s
