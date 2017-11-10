{-# LANGUAGE NoImplicitPrelude #-}
module Monads where

import Data.Monoid
import Prelude     hiding (Identity, Maybe (..), Monad, Reader, State, Writer)

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

newtype Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

newtype State s a = State {runState :: s -> (a, s)}

newtype Reader s a = Reader {runReader :: s -> a }

newtype Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where
  return = Identity
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  (State g) >>= f = State $ \s ->
    let (a, s') = g s
    in  runState (f a) s'

instance Monad (Reader s) where
  return = Reader . const
  (Reader g) >>= f = Reader $ \s -> runReader (f (g s)) s

instance Monoid w => Monad (Writer w) where
  return = Writer . (,) mempty
  (Writer (s, a)) >>= f = let Writer (s', b) = f a in Writer (mappend s s', b)
