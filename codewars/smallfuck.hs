-- > - Move pointer to the right (by 1 cell)
-- < - Move pointer to the left (by 1 cell)
-- * - Flip the bit at the current cell
-- [ - Jump past matching ] if value at current cell is 0
-- ] - Jump back to matching [ (if value at current cell is nonzero)

import Control.Monad.Reader
import Control.Monad.State
import Data.Array           as A
import Data.Foldable        as F (foldl', toList)
import Data.Map.Strict      as M (Map, empty, fromList, lookup, union)

data Bit = Zero | One deriving (Eq)

toChar :: Bit -> Char
toChar Zero = '0'
toChar One  = '1'

bFlip :: Bit -> Bit
bFlip Zero = One
bFlip One  = Zero

fromChar :: Char -> Bit
fromChar c | c == '0' = Zero
           | c == '1' = One
           | otherwise = error "impossible"

toArray :: [e] -> Array Int e
toArray l = listArray (0, length l - 1) l

type Brackets = Map Int Int

matchBracket :: Array Int Char -> Brackets
matchBracket a = fst . foldl' step (M.empty, []) . indices $ a
    where step :: (Brackets, [Int]) -> Int -> (Brackets, [Int])
          step acc @ (m, stk) idx = case a A.! idx of
            '[' -> (m, idx : stk)
            ']' -> case stk of
                h : t -> (m `union` fromList [(h, idx), (idx, h)], t)
                _     -> acc
            _   -> acc

data Env = Env (Array Int Char) Brackets

data S = S {
    getPC   :: Int,
    getDP   :: Int,
    getTape :: Array Int Bit
}
setPC :: (Int -> Int) -> S -> S
setPC f (S pc dp tape) = S (f pc) dp tape
setDP :: (Int -> Int) -> S -> S
setDP f (S pc dp tape) = S pc (f dp) tape
setTape :: (Array Int Bit -> Array Int Bit) -> S -> S
setTape f (S pc dp tape) = S pc dp (f tape)

type Interpreter = ReaderT Env (State S)

jump :: (Int -> Int) -> Interpreter String
jump f = do modify $ setPC f; go

next :: Interpreter String
next = jump (+1)

j :: Int -> Interpreter String
j pc = jump $ const pc

finish :: Interpreter String
finish = do
    (S _ _ tape) <- get
    return $ map toChar $ toList tape

flipAt :: Int -> Array Int Bit -> Array Int Bit
flipAt i a = a // [(i, bFlip $ a A.! i)]

go :: Interpreter String
go = do
    (Env code brackets) <- ask
    (S pc dp tape) <- get
    if not (inRange (bounds tape) dp) || not (inRange (bounds code) pc)
        then finish
        else case code A.! pc of
            '>' -> modify (setDP (+1)) >> next
            '<' -> modify (setDP (flip (-) $ 1)) >> next
            '*' -> modify (setTape $ flipAt dp) >> next
            '[' -> case tape A.! dp of
                Zero -> case M.lookup pc brackets of
                    Nothing    -> finish
                    Just match -> j match
                One -> next
            ']' -> case tape A.! dp of
                One -> case M.lookup pc brackets of
                    Nothing    -> finish
                    Just match -> j match
                Zero -> next
            _   -> next

interpreter :: String -> String -> String
interpreter code tape = fst $ runState (runReaderT go (Env (toArray code) (matchBracket $ toArray code))) (S 0 0 (fromChar `fmap` toArray tape))



main :: IO ()
main = do
    print $ interpreter "*>*>*>*>*>*>*>*" "00101100"
    print $ interpreter "*>*>>*>>>*>*" "00101100"
    print $ interpreter "[*][]*" "0"
    print $ interpreter "[>*]" "010"
