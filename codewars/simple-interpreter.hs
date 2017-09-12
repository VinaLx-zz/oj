module SimpleInteractiveInterpreter where

import           Control.Monad
import           Control.Monad.State          (StateT, lift, runStateT)
import qualified Control.Monad.State          as S
import           Data.Char
import           Data.Fixed                   (mod')
import qualified Data.Map                     as M
import           Debug.Trace
import           Text.ParserCombinators.ReadP as P
import           Text.Printf                  (printf)

data Operator = None | Plus | Minus | Mult | Div | Mod deriving (Eq, Show)

fromOp :: Operator -> Double -> Double -> Double
fromOp op = case op of
    Plus  -> (+)
    Minus -> (-)
    Mult  -> (*)
    Div   -> (/)
    Mod   -> mod'

precedence :: Operator -> Int
precedence Plus  = 10
precedence Minus = 10
precedence Mult  = 20
precedence Div   = 20
precedence Mod   = 20

isOperator :: Char -> Bool
isOperator = flip elem "+-*/%"

fromChar :: Char -> Operator
fromChar '+' = Plus
fromChar '-' = Minus
fromChar '*' = Mult
fromChar '/' = Div
fromChar '%' = Mod

data Expression = Assign String Expression
                | Number Double
                | Id String
                | Call String [Expression]
                | Op Expression Operator Expression
                deriving (Show)

data Func = Func String [String] Expression deriving (Show)

data Value = Val Double | Fn Func deriving (Show)

type Env = M.Map String Value

isFunc :: Env -> String -> Bool
isFunc env name =
    case name `M.lookup` env of
        Just (Fn _) -> True
        _           -> False

isVal :: Env -> String -> Bool
isVal env name =
    case name `M.lookup` env of
        Just (Val _) -> True
        _            -> False

arity :: Func -> Int
arity (Func _ ps _) = length ps

type Result = Maybe Double

newtype Interpreter = Interpreter Env deriving (Show)

-- parser

space :: P.ReadP ()
space = () <$ satisfy (`elem` "\n\r\t ")

spaces :: P.ReadP ()
spaces = () <$ P.many space

spaces1 :: P.ReadP ()
spaces1 = () <$ P.many1 space

symbolC :: Char -> P.ReadP Char
symbolC c = between spaces spaces (char c)

number :: P.ReadP Expression
number = do
    ints <- many1 digit
    frac <- option "" $ ('.' :) <$> (char '.' >> many1 digit)
    return (Number $ read $ ints ++ frac)
    where digit = satisfy isDigit

identifier :: P.ReadP String
identifier = do
    h <- satisfy $ \c -> isAlpha c || c == '_'
    t <- P.many $ satisfy $ \c -> isAlphaNum c || c == '_'
    return (h : t)

identExpr :: P.ReadP Expression
identExpr = Id <$> identifier

identSatisfy :: (String -> Bool) -> P.ReadP String
identSatisfy p = do
    ident <- identifier
    if p ident
        then return ident
        else fail "identifier is not function"

assignment :: Env -> P.ReadP Expression
assignment env = do
    ident <- identSatisfy (not . isFunc env)
    symbolC '='
    expr <- expression env
    return $ Assign ident expr

inParen :: Env -> P.ReadP Expression
inParen env = between (symbolC '(') (symbolC ')') (expression env)

operator :: P.ReadP Operator
operator = fromChar <$> satisfy isOperator

operator' :: P.ReadP Operator
operator' = between spaces spaces operator

fnCall :: Env -> P.ReadP Expression
fnCall env = do
    ident <- identSatisfy (isFunc env)
    let Just (Fn func) = M.lookup ident env
        n = arity func
    exprs <- replicateM n (expression' env)
    return $ Call ident exprs

primary :: Env -> P.ReadP Expression
primary env = number +++ inParen env +++
    assignment env +++ fnCall env <++ (Id <$> identSatisfy (isVal env))

primary' :: Env -> P.ReadP Expression
primary' env = P.between spaces spaces (primary env)

opExpr :: Env -> P.ReadP Expression
opExpr env = do
    expr <- primary' env
    opExpr' expr 0
    where opExpr' :: Expression -> Int -> P.ReadP Expression
          opExpr' lhs mp = do
            mop <- lookAhead
            case mop of
                Just op | precedence op >= mp -> maintain lhs mp
                _       -> return lhs
          maintain :: Expression -> Int -> P.ReadP Expression
          maintain lhs mp = do
            op <- operator'
            rhs <- primary' env
            rhs' <- tryClimb rhs (precedence op)
            let lhs' = Op lhs op rhs'
            mop <- lookAhead
            case mop of
                Just op' | precedence op' >= mp -> maintain lhs' mp
                _        -> return lhs'
          tryClimb :: Expression -> Int -> P.ReadP Expression
          tryClimb rhs mp = do
            mop <- lookAhead
            case mop of
                Just op | precedence op > mp -> do
                    rhs' <- opExpr' rhs (precedence op)
                    tryClimb rhs' mp
                _ -> return rhs
          lookAhead :: P.ReadP (Maybe Operator)
          lookAhead = do
            s <- P.look
            return $ nextOp s
          nextOp :: String -> Maybe Operator
          nextOp s = case dropWhile isSpace s of
            c : _ | isOperator c -> Just $ fromChar c
            _     -> Nothing

expression :: Env -> P.ReadP Expression
expression env = opExpr env <++ primary env

expression' :: Env -> P.ReadP Expression
expression' env = P.between spaces spaces (expression env)

fn :: P.ReadP Func
fn = do
    string "fn" >> spaces
    name <- identifier
    params <- spaces1 >> paramList
    spaces >> string "=>"
    expr <- expression $ dummyEnv params
    return $ Func name params expr
    where paramList = identifier `sepBy` spaces
          dummyEnv = M.fromList . fmap (\p -> (p, Val 0))

data Parse = F Func | E Expression | Empty

empty :: P.ReadP Parse
empty = Empty <$ spaces

allParse :: Env -> P.ReadP Parse
allParse env = between spaces spaces $ (F <$> fn) +++ (E <$> expression env) <++ empty

runParser :: Env -> String -> Either String Parse
runParser env s = case (null . snd) `filter` P.readP_to_S (allParse env) s of
    (p, _) : _ -> Right p
    _          -> Left "parse fail"

-- evaluator

type Evaluator = StateT Env (Either String)

runEval :: Evaluator a -> Env -> Either String (a, Env)
runEval = runStateT

applyFunc :: Func -> [Double] -> Either String Double
applyFunc (Func _ params expr) args =
    if length params /= length args
        then Left "incorrect arguments"
        else fst <$>
            runEval (eval expr) (M.fromList $ zip params (Val <$> args))

eval :: Expression -> Evaluator Double
eval (Assign name expr) = do
    env <- S.get
    case name `M.lookup` env of
        Just (Fn _) -> lift $ Left $
            printf "attemp to override function '%s'" name
        _ -> do
            val <- eval expr
            S.modify $ M.insert name (Val val)
            return val
eval (Number n) = return n
eval (Id name) = do
    env <- S.get
    case name `M.lookup` env of
        Just (Val v) -> return v
        Just _ -> lift $ Left $
            printf "evaluation of '%s' leads to a function" name
        _ -> lift $ Left $
            printf "identifier '%s' not found" name
eval (Call name args) = do
    env <- S.get
    case name `M.lookup` env of
        Just (Fn func) -> do
            as <- traverse eval args
            lift $ applyFunc func as
        Just _ -> lift $ Left $ printf "identifier '%s' is a number" name
        Nothing -> lift $ Left $ printf "identifier '%s' not found" name
eval (Op lhs op rhs) = do
    x <- eval lhs
    y <- eval rhs
    return $ fromOp op x y

evalAll :: Parse -> Evaluator Result
evalAll (F f) = addFunc f
evalAll (E e) = Just <$> eval e
evalAll Empty = return Nothing

addFunc :: Func -> Evaluator Result
addFunc f @ (Func name _ _) = do
    env <- S.get
    if isVal env name
        then lift $ Left $ printf "identifier '%s' is a number" name
        else S.modify (M.insert name (Fn f))
    return Nothing

newInterpreter :: Interpreter
newInterpreter = Interpreter M.empty

input :: String -> Interpreter -> Either String (Result, Interpreter)
input s (Interpreter env) = do
    p <- runParser env s
    (r, env') <- runEval (evalAll p) env
    return (r, Interpreter env')
