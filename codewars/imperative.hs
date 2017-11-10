module Imperative (def, var, lit, while, (+=), (-=), (*=)) where

import Control.Monad
import Data.IORef
import System.IO.Unsafe

type Imp = IO
type Var = IORef Integer

def :: Imp Var -> Integer
def program = unsafePerformIO $ do
    result <- program
    readIORef result

var :: Integer -> Imp Var
var = newIORef

lit :: Integer -> Var
lit = unsafePerformIO . newIORef

while :: Var -> (Integer -> Bool) -> Imp () -> Imp ()
while r p act = do
    i <- readIORef r
    when (p i) (act >> while r p act)

(+=) :: Var -> Var -> Imp ()
a += b = do
    i <- readIORef a
    j <- readIORef b
    writeIORef a (i + j)

(-=) :: Var -> Var -> Imp ()
a -= b = do
    i <- readIORef a
    j <- readIORef b
    writeIORef a (i - j)

(*=) :: Var -> Var -> Imp ()
a *= b = do
    i <- readIORef a
    j <- readIORef b
    writeIORef a (i * j)

howManyBetween :: Integer -> Integer -> Integer
howManyBetween c n = def $ do
    result <- var 0
    i      <- var (c + 1)
    while i (<n) $ do
        result += lit 1
        i      += lit 1
    return result

factorial :: Integer -> Integer
factorial n = def $ do
    result <- var 1
    i      <- var n
    while i (>0) $ do
        result *= i
        i      -= lit 1
    return result
