module TinyThreePassCompiler where

import           Control.Monad.State
import qualified Data.Map.Strict     as M
import           Text.Printf         (printf)

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)

alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c:cs)
    | c `elem` "-+*/()[]" = TChar c : tokenize cs
    | not (null i) = TInt (read i) : tokenize is
    | not (null s) = TStr s : tokenize ss
    | otherwise = tokenize cs
    where
    (i, is) = span (`elem` digit) xxs
    (s, ss) = span (`elem` alpha) xxs

compile :: String -> [String]
compile = pass3 . pass2 . pass1

data S = S {args :: M.Map String Int, tokens :: [Token]}

lookAhead :: Int -> State S [Token]
lookAhead n = do
    S {tokens = ts} <- get
    return $ take n ts

advance :: Int -> State S [Token]
advance n = do
    s @ S {tokens = ts} <- get
    put s {tokens = drop n ts}
    return $ take n ts

parseArgList :: State S ()
parseArgList = do
    [TChar '['] <- advance 1
    parseArgList' 0
    [TChar ']'] <- advance 1
    return ()
    where parseArgList' :: Int -> State S ()
          parseArgList' n = do
            [t] <- lookAhead 1
            case t of
                TChar ']' -> return ()
                TStr name -> do
                    s @ S {args = as} <- get
                    put s {args = M.insert name n as}
                    advance 1
                    parseArgList' (succ n)

isOp :: Char -> Bool
isOp = flip elem "+-*/"

precedence :: Char -> Int
precedence '+' = 0
precedence '-' = 0
precedence '*' = 1
precedence '/' = 1
precedence c   = error $ printf "Unexpected operator %c" c

opToF :: Char -> (Int -> Int -> Int)
opToF '+' = (+)
opToF '-' = (-)
opToF '*' = (*)
opToF '/' = div

applyOp :: Char -> AST -> AST -> AST
applyOp '+' = Add
applyOp '-' = Sub
applyOp '*' = Mul
applyOp '/' = Div
applyOp c   = error $ printf "Unexpected operator %c" c

parseExpr :: State S AST
parseExpr = do
    lhs <- parsePrimary
    parseWithPrec lhs 0
    where parseWithPrec :: AST -> Int -> State S AST
          parseWithPrec lhs minPrec = do
            l <- lookAhead 1
            case l of
                [TChar op] | isOp op && precedence op >= minPrec -> do
                    advance 1
                    rhs <- parsePrimary
                    rhs' <- parseWithPrec' rhs (precedence op)
                    parseWithPrec (applyOp op lhs rhs') minPrec
                _ -> return lhs
          parseWithPrec' :: AST -> Int -> State S AST
          parseWithPrec' rhs prec = do
            l <- lookAhead 1
            case l of
                [TChar op] | isOp op && precedence op > prec -> do
                    rhs' <- parseWithPrec rhs (precedence op)
                    parseWithPrec' rhs' prec
                _ -> return rhs
          parsePrimary :: State S AST
          parsePrimary = do
            [t] <- advance 1
            S {args = a} <- get
            case t of
                TStr name -> return $ Arg $ a M.! name
                TInt i    -> return $ Imm i
                TChar '(' -> do
                    ast <- parseExpr
                    [TChar ')'] <- advance 1
                    return ast

parser :: State S AST
parser = do
    parseArgList
    parseExpr

parse :: [Token] -> AST
parse ts = evalState parser S {tokens = ts, args = M.empty}

pass1 :: String -> AST
pass1 = parse . tokenize

foldConst :: AST -> AST
foldConst ast =
    case ast of
        i @ (Imm _) -> i
        a @ (Arg _) -> a
        (Add l r)   -> foldOp '+' l r
        (Sub l r)   -> foldOp '-' l r
        (Mul l r)   -> foldOp '*' l r
        (Div l r)   -> foldOp '/' l r
    where foldOp :: Char -> AST -> AST -> AST
          foldOp op lhs rhs =
            let lhs' = foldConst lhs
                rhs' = foldConst rhs
            in case (lhs', rhs') of
                (Imm x, Imm y) -> Imm (opToF op x y)
                _              -> applyOp op lhs' rhs'

pass2 :: AST -> AST
pass2 = foldConst

codeGen :: AST -> [String]
codeGen ast = case ast of
    (Imm i)   -> [printf "IM %d" i, "PU"]
    (Arg n)   -> [printf "AR %d" n, "PU"]
    (Add l r) -> genExpr "AD" l r
    (Sub l r) -> genExpr "SU" l r
    (Mul l r) -> genExpr "MU" l r
    (Div l r) -> genExpr "DI" l r
    where genExpr :: String -> AST -> AST -> [String]
          genExpr op lhs rhs =
            codeGen lhs ++ codeGen rhs ++ ["PO", "SW", "PO", op, "PU"]

pass3 :: AST -> [String]
pass3 = codeGen

simulate :: [String] -> [Int] -> Int
simulate asm argv = takeR0 $ foldl step (0, 0, []) asm where
  step (r0,r1,stack) ins =
    case ins of
      ('I':'M':xs) -> (read xs, r1, stack)
      ('A':'R':xs) -> (argv !! n, r1, stack) where n = read xs
      "SW" -> (r1, r0, stack)
      "PU" -> (r0, r1, r0:stack)
      "PO" -> (head stack, r1, tail stack)
      "AD" -> (r0 + r1, r1, stack)
      "SU" -> (r0 - r1, r1, stack)
      "MU" -> (r0 * r1, r1, stack)
      "DI" -> (r0 `div` r1, r1, stack)
  takeR0 (r0,_,_) = r0
