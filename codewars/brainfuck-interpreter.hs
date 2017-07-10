import Control.Monad.Reader as Reader
import Control.Monad.State  as State
import Control.Monad.Writer as Writer
import Data.Array           as A
import Data.Char            (chr, ord)
import Data.Foldable        (foldl')
import Data.Map             as M hiding (foldl')

type Byte = Int
type Ptr = Int

inc :: Byte -> Byte
inc b = (b + 1) `mod` 256

dec :: Byte -> Byte
dec b = (b + 256 - 1) `mod` 256

type Memory = Map Ptr Byte

memDo :: Memory -> Ptr -> (Byte -> Byte) -> Memory
memDo mem addr f =
    insert addr (f (findWithDefault 0 addr mem)) mem

memFind :: Memory -> Ptr -> Byte
memFind mem addr = findWithDefault 0 addr mem

memSet :: Memory -> Ptr -> Byte -> Memory
memSet mem addr b = insert addr b mem

data S = S Memory Ptr Ptr [Byte]

emptyStateWithInput :: String -> S
emptyStateWithInput str = S M.empty 0 0 (fmap ord str)

type Code = Array Ptr Char

data Env = Env Code (Map Ptr Ptr)

fromCode :: Code -> Map Ptr Ptr
fromCode arr = snd $ foldl' step ([], M.empty) (indices arr)
    where step :: ([Ptr], Map Ptr Ptr) -> Ptr -> ([Ptr], Map Ptr Ptr)
          step acc @ (stk, m) idx = case arr A.! idx of
              '[' -> (idx : stk, m)
              ']' -> case stk of
                  h : rest -> (rest, m `union` M.fromList [(idx, h), (h, idx)])
                  [] -> (stk, m)
              _   -> acc

envFromCode :: Code -> Env
envFromCode c = Env c (fromCode c)


type Output = [Int]
type Interpreter = WriterT Output (ReaderT Env (State S))

runInterpreter :: Env -> S -> Interpreter Bool -> Maybe String
runInterpreter env s i =
    let ((status, output), _) = (flip runState) s . (flip runReaderT) env . runWriterT $ i
    in  if status
           then Just $ intListToString output
           else Nothing

interpretSuccess :: Interpreter Bool
interpretSuccess = return True

interpretFailure :: Interpreter Bool
interpretFailure = return False

jump :: (Ptr -> Ptr) -> Interpreter Bool
jump f = do
    S mem pc mp input <- State.get
    State.put $ S mem (f pc) mp input
    interpret

incPC :: Interpreter Bool
incPC = jump (+ 1)

setPC :: Ptr -> Interpreter Bool
setPC pc = jump (const pc)

interpret :: Interpreter Bool
interpret = do
    Env code bracket <- Reader.ask
    S mem pc mp input <- State.get
    let curVal = memFind mem mp
    if not $ inRange (bounds code) pc
        then interpretSuccess
        else case code A.! pc of
            '>' -> (State.put $ S mem pc (mp + 1) input) >> incPC
            '<' -> (State.put $ S mem pc (mp - 1) input) >> incPC
            '+' -> (State.put $ S (memDo mem mp inc) pc mp input) >> incPC
            '-' -> (State.put $ S (memDo mem mp dec) pc mp input) >> incPC
            '.' -> Writer.tell [curVal] >> incPC
            ',' -> case input of
                []        -> interpretFailure
                b : other -> (State.put $ S (memSet mem mp b) pc mp other) >> incPC
            '[' -> if curVal /= 0
                then incPC
                else case M.lookup pc bracket of
                    Nothing    -> interpretFailure
                    Just match -> setPC match
            ']' -> if curVal == 0
                then incPC
                else case M.lookup pc bracket of
                    Nothing    -> interpretFailure
                    Just match -> setPC match

stringToArray :: String -> Code
stringToArray s = listArray (0, length s - 1) s

intListToString :: [Int] -> String
intListToString = fmap chr

stringToIntList :: String -> [Int]
stringToIntList = fmap ord

executeString :: String -> String -> Maybe String
executeString source input =
    runInterpreter env s interpret
    where env = envFromCode (stringToArray source)
          s = emptyStateWithInput input
