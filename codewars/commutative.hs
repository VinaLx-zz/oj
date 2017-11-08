{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Kata.AdditionCommutes ( plusCommutes ) where

-- These are some lemmas that may be helpful.
-- They will *not* be tested, so rename them
-- if you so desire. Good luck!

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ     = EqlZ
reflexive (NumS n) = EqlS $ reflexive n

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ      = EqlZ
symmetric (EqlS eq) = EqlS $ symmetric eq

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ             = EqlZ
transitive (EqlS lhs) (EqlS rhs) = EqlS $ transitive lhs rhs

plusSRightS :: Natural a -> Natural b -> Equal (S (a :+: b)) (a :+: S b)
plusSRightS NumZ NumZ     = EqlS EqlZ
plusSRightS NumZ (NumS n) = EqlS $ plusSRightS NumZ n
plusSRightS (NumS n) m    = EqlS $ plusSRightS n m

-- This is the proof that the kata requires.
-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ NumZ     = EqlZ
plusCommutes NumZ (NumS n) = EqlS $ plusCommutes NumZ n
plusCommutes (NumS n) m = transitive (EqlS $ plusCommutes n m) (plusSRightS m n)

-- Equal (a + b) (b + a) => Equal (S (a + b)) (S (b + a)) => ... => Equal (S a + b) (b + S a)
--  ... => Equal (S (b + a)) (b + S a)

-- For reference, here are the definitions, if you
-- want to copy them into an IDE:

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
    NumZ :: Natural Z
    NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)
