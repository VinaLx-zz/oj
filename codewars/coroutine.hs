{-# LANGUAGE DeriveFunctor #-}
module Coroutine where

import Control.Monad (ap, forever, (>=>))

-- Preloaded contains the following:
--
newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)

data Command r u d a = Done a
                     | Out d (Coroutine r u d a)
                     | In (u -> Coroutine r u d a) deriving Functor

-- Useful alias
apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

instance Applicative (Coroutine r u d) where
    pure = return
    (<*>) = ap

instance Monad (Coroutine r u d) where
    return x = Coroutine ($ Done x)
    (Coroutine f) >>= g  = Coroutine $ \k -> f $ \cmd ->
        case cmd of
            Done a  -> runCoroutine (g a) k
            Out d c -> k $ Out d (c >>= g)
            In i    -> k $ In $ i >=> g

(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
cl @ (Coroutine lhs) >>> cr @ (Coroutine rhs) = Coroutine $ \k -> rhs $ \cmdr ->
    case cmdr of
        Done a  -> k $ Done a
        Out d c -> k $ Out d $ cl >>> c
        In i    -> lhs $ \cmdl ->
            case cmdl of
                Done a    -> k $ Done a
                In ui     -> k $ In $ \u -> ui u >>> cr
                Out m cl' -> runCoroutine (cl' >>> i m) k

-- It might be useful to define the following function
-- pipe2 :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a

-- Library functions

output :: a -> Coroutine r u a ()
output v = Coroutine $ \k -> k $ Out v $ return ()

input :: Coroutine r v d v
input = Coroutine $ \k -> k $ In return

produce :: [a] -> Coroutine r u a ()
produce []       = return ()
produce (a : as) = Coroutine $ \k -> k $ Out a $ produce as

consume :: Coroutine [t] u t a -> [t]
consume (Coroutine f) = f $ \cmd ->
    case cmd of
        Done _  -> []
        Out t c -> t : consume c
        In uf   -> []

filterC :: (v -> Bool) -> Coroutine r v v ()
filterC p = Coroutine $ \k -> k $ In $ \v ->
    if p v then output v >> filterC p else filterC p

limit :: Int -> Coroutine r v v ()
limit n = if n <= 0 then return () else do
    i <- input
    output i
    limit (n - 1)

suppress :: Int -> Coroutine r v v ()
suppress n = do
    i <- input
    if n > 0 then suppress (n - 1) else output i >> suppress n

add :: Coroutine r Int Int ()
add = forever $ do
    i <- input
    j <- input
    output $ i + j

duplicate :: Coroutine r v v ()
duplicate = forever $ do
    i <- input
    output i
    output i

-- Programs
-- 1. A program which outputs the first 5 even numbers of a stream.
-- 2. A program which produces a stream of the triangle numbers
-- 3. A program which multiplies a stream by 2
-- 4. A program which sums adjacent pairs of integers

p1, p2, p3, p4 :: Coroutine r Int Int ()

p1 = filterC even >>> limit 5
p2 = produce [n * (n + 1) `div` 2 | n <- [1..]]
p3 = forever $ (*2) <$> input >>= output
p4 = do
    i <- input
    p4' i
    where p4' i = do
            i2 <- input
            output (i + i2)
            p4' i2
