module Befunge93 where

import Prelude hiding (Left, Right)

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Array           as A
import           Data.Char
import           Data.List            (intercalate)
import           System.Random        (StdGen)
import qualified System.Random        as R (next)

type Board = A.Array (Int, Int) Char

data Direction = Up | Down | Left | Right deriving Show

intToDir :: Int -> Direction
intToDir n | n `mod` 4 == 0 = Right
           | n `mod` 4 == 1 = Left
           | n `mod` 4 == 2 = Down
           | n `mod` 4 == 3 = Up

theta :: Direction -> (Int, Int)
theta Up    = (-1, 0)
theta Down  = (1, 0)
theta Left  = (0, -1)
theta Right = (0, 1)

data Process = Process {
    board      :: Board,
    position   :: (Int, Int),
    direction  :: Direction,
    stack      :: [Int],
    stringMode :: Bool,
    generator  :: StdGen
} deriving Show

type Interpreter = WriterT String (State Process)

bDiv :: Int -> Int -> Int
bDiv _ 0 = 0
bDiv a b = div a b

bMod :: Int -> Int -> Int
bMod _ 0 = 0
bMod a b = mod a b

bGT :: Int -> Int -> Int
bGT a b = if a > b then 1 else 0

bNot :: Int -> Int
bNot 0 = 1
bNot _ = 0

stackOp :: (Int -> Int -> Int) -> [Int] -> [Int]
stackOp f (rhs : lhs : xs) = f lhs rhs : xs

stackOp1 :: (Int -> Int) -> [Int] -> [Int]
stackOp1 f (x : xs) = f x : xs

bDup :: [Int] -> [Int]
bDup (x : xs) = x : x : xs
bDup []       = [0]

bSwap :: [Int] -> [Int]
bSwap (x : y : xs) = y : x : xs
bSwap [x]          = [0, x]
bSwap []           = []

bPut :: [Int] -> ([Int], Char, (Int, Int))
bPut (x : y : v : xs) = (xs, chr v, (x, y))

bGet :: [Int] -> ([Int], (Int, Int))
bGet (x : y : xs) = (xs, (x, y))

next :: Interpreter ()
next = do
    s @ Process {board = b, direction = d, position = p} <- get
    put $ s {position = move b d p}
    interpreter

interpreter :: WriterT String (State Process) ()
interpreter = do
    s @ (Process b pos dir stk str gen) <- get
    let c = b A.! pos
    case c of
        _ | str && c /= '"' -> put (s {stack = ord c : stk}) >> next
        '@' -> return ()
        _ -> do
            case c of
                _   | isDigit c -> put $ s {stack = ord c - ord '0' : stk}
                '+' -> put $ s {stack = stackOp (+) stk}
                '-' -> put $ s {stack = stackOp (-) stk}
                '*' -> put $ s {stack = stackOp (*) stk}
                '/' -> put $ s {stack = stackOp bDiv stk}
                '%' -> put $ s {stack = stackOp bMod stk}
                '!' -> put $ s {stack = stackOp1 bNot stk}
                '`' -> put $ s {stack = stackOp bGT stk}
                '>' -> put $ s {direction = Right}
                '<' -> put $ s {direction = Left}
                '^' -> put $ s {direction = Up}
                'v' -> put $ s {direction = Down}
                '_' -> put $ s {direction = if head stk == 0 then Right else Left, stack = tail stk}
                '|' -> put $ s {direction = if head stk == 0 then Down else Up, stack = tail stk}
                '"' -> put $ s {stringMode = not str}
                ':' -> put $ s {stack = bDup stk}
                '\\' -> put $ s {stack = bSwap stk}
                '$' -> put $ s {stack = tail stk}
                '.' -> tell (show $ head stk) >> put (s {stack = tail stk})
                ',' -> tell (return $ chr $ head stk) >> put (s {stack = tail stk})
                '#' -> put $ s {position = move b dir pos}
                'p' -> let (stk', putC, putPos) = bPut stk
                       in  put $ s {stack = stk', board = b A.// [(putPos, putC)]}
                'g' -> let (stk', getPos) = bGet stk
                       in  put $ s {stack = ord (b A.! getPos) : stk'}
                '?' -> let (i, g) = R.next gen
                       in  put $ s {direction = intToDir i, generator = g}
                ' ' -> pure ()
                _ -> error "unknown command"
            next

runInterpreter :: Board -> StdGen -> String
runInterpreter b gen = snd $
    evalState (runWriterT interpreter) (Process b (0, 0) Right [] False gen)

move :: Board -> Direction -> (Int, Int) -> (Int, Int)
move b d (x, y) =
    let (dx, dy) = theta d
        (down, right) = snd $ bounds b
        (lx, ly) = (down + 1, right + 1)
    in  ((x + lx + dx) `mod` lx, (y + ly + dy) `mod` ly)

fillMatrix :: [String] -> [String]
fillMatrix a =
    let emptyLine = replicate 80 ' '
        emptyLines = replicate (25 - length a) emptyLine
    in  map (\s -> s ++ replicate (80 - length s) ' ') a ++ emptyLines

stringToBoard :: String -> Board
stringToBoard s =
    let m = fillMatrix $ lines s
        size = (length m - 1, length (head m) - 1)
    in  A.listArray ((0, 0), size) $ intercalate [] m

interpret :: StdGen -> String -> String
interpret gen bs = runInterpreter (stringToBoard bs) gen
