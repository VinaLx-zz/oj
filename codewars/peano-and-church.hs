{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module PC where

import Data.List

type ISO a b = (a -> b, b -> a)

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 (ab, ba) = (\aaa b1 b2 -> ab (aaa (ba b1) (ba b2)), \bbb a1 a2 -> ba (bbb (ab a1) (ab a2)))

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natrual Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natrual Number
-- Since Haskell is lazy, we also have infinity

class Nat n where
    zero :: n
    successor :: n -> n
    nat :: a -> (n -> a) -> n -> a -- Pattern Matching
    iter :: a -> (a -> a) -> n -> a -- Induction
    plus, minus, mult, pow :: n -> n -> n

    inf :: n
    inf = successor inf

    divide :: n -> n -> n
    l `divide` r | l == 0 && r == 0 = undefined
    l `divide` r | l < r = 0
    l `divide` r  = successor $ (l `minus` r) `divide` r
    -- notice (l `divide` 0) when l is not 0 will return inf
    isoP :: ISO n Peano -- See below for the definition of Peano
    isoP = (iter zero successor, iter zero successor)
    toP :: n -> Peano
    toP = substL isoP

instance {-# OVERLAPPABLE #-} Nat n => Show n where
    show = show . toP

instance {-# OVERLAPPABLE #-} Nat n => Eq n where
    l == r = toP l == toP r

instance {-# OVERLAPPABLE #-} Nat n => Ord n where
    l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-} Nat n => Num n where
    abs = id
    signum = nat zero (const 1)
    fromInteger 0 = zero
    fromInteger n | n > 0 = successor $ fromInteger (n - 1)
    (+) = plus
    (*) = mult
    (-) = minus

-- We can encode Natrual Number directly as Algebraic Data Type(ADT).
data Peano = O | S Peano deriving (Show, Eq, Ord)

-- Remember, 0 - x = 0 for all x.
instance Nat Peano where
    zero = O
    successor = S

    nat a _ O     = a
    nat _ f (S n) = f n

    iter a _ O     = a
    iter a f (S n) = f $ iter a f n

    plus O n     = n
    plus (S n) m = S $ n `plus` m

    minus O _         = O
    minus n O         = n
    minus (S n) (S m) = minus n m

    mult O n     = O
    mult (S n) m = plus m $ mult n m

    pow n O     = S O
    pow n (S m) = mult n $ pow n m

-- Peano is very similar to a basic data type in Haskell - List!
-- O is like [], and S is like :: (except it lack the head part)
-- When we want to store no information, we can use (), a empty tuple
-- This is different from storing nothing (called Void in Haskell),
-- as we can create a value of () by using (),
-- but we cannot create a value of Void.

-- Notice how you can implement everything once you have isoP,
-- By converting to Peano and using Nat Peano?
-- Dont do that. You wont learn anything.
-- Try to use operation specific to list.
instance Nat [()] where
    zero = []
    successor = (() :)

    nat a _ []      = a
    nat _ f (_ : t) = f t

    iter a f = foldr (const f) a

    plus = (++)

    minus [] _            = []
    minus l []            = l
    minus (_ : m) (_ : n) = minus m n

    mult [] _      = []
    mult (_ : n) m = plus m $ mult n m

    pow _ []      = [()]
    pow n (_ : m) = mult n $ pow n m

-- Instead of defining Nat from zero, sucessor (and get Peano),
-- We can define it from Pattern Matching
newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }
instance Nat Scott where
    zero = Scott const

    -- iter :: a -> (a -> a) -> n -> a -- Induction

    successor n = Scott $ \a f -> f n

    nat a f (Scott n) = n a f

    iter a f (Scott n) = n a $ \n -> f $ iter a f n

    -- Other operation on Scott numeral is sort of boring,
    -- So we implement it using operation on Peano.
    -- You shouldnt do this - I had handled all the boring case for you.
    plus = substR (liftISO2 isoP) plus
    minus = substR (liftISO2 isoP) minus
    mult = substR (liftISO2 isoP) mult
    pow = substR (liftISO2 isoP) pow

-- Or from induction!
newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }

cPred :: Church -> Church
cPred (Church n) = Church $ \f x -> n (\g h -> h (g f)) (const x) id

isZero :: Church -> Bool
isZero (Church n) = n (const False) True

instance Nat Church where
    zero = Church $ const id
    successor (Church n) = Church $ \f a -> f $ n f a

    nat a f n | isZero n = a
    nat a f n = f (cPred n)

    iter a f (Church n) = n f a

    plus (Church n) (Church m) = Church $ \f a -> n f $ m f a

    minus n _ | isZero n = zero
    minus n m | isZero m = n
    minus n m = minus (cPred n) (cPred m)

    mult (Church n) (Church m) = Church $ \f -> n $ m f

    pow (Church n) (Church m) = Church $ m n
