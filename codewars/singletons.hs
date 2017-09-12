{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Singletons where

import Prelude hiding (drop, head, map, replicate, tail, take, zipWith, (++))

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- promoted to type level by data kinds
data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool
type instance m :< Zero = False
type instance Zero :< Succ n = True
type instance Succ m :< Succ n = m :< n

type family (Add (a :: Nat) (b :: Nat)) :: Nat
type instance Add Zero n = n
type instance Add (Succ m) n = Succ (Add m n)

type family (Minus (a :: Nat) (b :: Nat)) :: Nat
type instance Minus n Zero = n
type instance Minus (Succ m) (Succ n) = Minus m n
type instance Minus Zero n = Zero

type family (Min (a :: Nat) (b :: Nat)) :: Nat
type instance Min Zero n = Zero
type instance Min n Zero = Zero
type instance Min (Succ n) (Succ m) = Succ (Min n m)
-- to be defined

map :: (a -> b) -> Vec a n -> Vec b n
map _ VNil         = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index SZero (VCons x _)      = x
index (SSucc n) (VCons _ xs) = index n xs

replicate :: s -> SNat a -> Vec s a
replicate _ SZero     = VNil
replicate s (SSucc n) = VCons s (replicate s n)

-- Both vectors must be of equal length
zipWith :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n
zipWith _ VNil VNil                 = VNil
zipWith f (VCons a as) (VCons b bs) = VCons (f a b) (zipWith f as bs)
-- zipWith = undefined

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
VNil ++ b = b
VCons a as ++ b = VCons a (as ++ b)

-- The semantics should match that of take for normal lists.
take :: SNat a -> Vec s b -> Vec s (Min a b)
take SZero _                = VNil
take (SSucc n) (VCons a as) = VCons a (take n as)
take _ VNil                 = VNil

-- The semantics should match that of drop for normal lists.
drop :: SNat a -> Vec s b -> Vec s (Minus b a)
drop SZero v                = v
drop (SSucc n) (VCons x xs) = drop n xs
drop _ VNil                 = VNil

head :: Vec a (Succ n) -> a
head (VCons a _) = a

tail :: Vec a (Succ n) -> Vec a n
tail (VCons a as) = as
