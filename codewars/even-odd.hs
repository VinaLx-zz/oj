{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module OddsAndEvens where

-- | The natural numbers.
data Nat = Z | S Nat

-- | The axioms of even numbers.
data Even (a :: Nat) :: * where
    -- | Zero is even.
    ZeroEven :: Even Z
    -- | If n is even, then n+2 is even.
    NextEven :: Even n -> Even (S (S n))

-- | The axioms of odd numbers.
data Odd (a :: Nat) :: * where
    -- | One is odd.
    OneOdd :: Odd (S Z)
    -- | If n is odd, then n+2 is odd.
    NextOdd :: Odd n -> Odd (S (S n))

-- | Proves that if n is even, n+1 is odd.
-- Notice how I use the axioms here.
evenPlusOne :: Even n -> Odd (S n)
evenPlusOne ZeroEven     = OneOdd
evenPlusOne (NextEven n) = NextOdd (evenPlusOne n)

-- | Proves that if n is odd, n+1 is even.
oddPlusOne :: Odd n -> Even (S n)
oddPlusOne OneOdd      = NextEven ZeroEven
oddPlusOne (NextOdd n) = NextEven (oddPlusOne n)

-- | Adds two natural numbers together.
-- Notice how the definition pattern matches.
type family   Add (n :: Nat) (m :: Nat) :: Nat
type instance Add Z m = m
type instance Add (S n) m = S (Add n m)

-- | Proves even + even = even
-- Notice how the pattern matching mirrors `Add`s definition.
evenPlusEven :: Even n -> Even m -> Even (Add n m)
evenPlusEven ZeroEven m     = m
evenPlusEven (NextEven n) m = NextEven (evenPlusEven n m)

-- | Proves odd + odd = even
oddPlusOdd :: Odd n -> Odd m -> Even (Add n m)
oddPlusOdd OneOdd n      = oddPlusOne n
oddPlusOdd (NextOdd n) m = NextEven (oddPlusOdd n m)

-- | Proves even + odd = odd
evenPlusOdd :: Even n -> Odd m -> Odd (Add n m)
evenPlusOdd ZeroEven o     = o
evenPlusOdd (NextEven e) o = NextOdd (evenPlusOdd e o)

-- | Proves odd + even = odd
oddPlusEven :: Odd n -> Even m -> Odd (Add n m)
oddPlusEven OneOdd e      = evenPlusOne e
oddPlusEven (NextOdd o) e = NextOdd (oddPlusEven o e)

-- | Multiplies two natural numbers.
type family   Mult (n :: Nat) (m :: Nat) :: Nat
type instance Mult Z n = Z
type instance Mult (S n) m = Add (Mult n m) m
-- type instance Mult n m = undefined -- TODO: Mult n m

-- | Proves even * even = even
evenTimesEven :: Even n -> Even m -> Even (Mult n m)
evenTimesEven ZeroEven _      = ZeroEven
evenTimesEven (NextEven e) e' =
    evenPlusEven (evenPlusEven (evenTimesEven e e') e') e'

-- | Proves odd * odd = odd
oddTimesOdd :: Odd n -> Odd m -> Odd (Mult n m)
oddTimesOdd OneOdd o       = o
oddTimesOdd (NextOdd o) o' = evenPlusOdd (oddPlusOdd (oddTimesOdd o o') o') o'

-- | Proves even * odd = even
evenTimesOdd :: Even n -> Odd m -> Even (Mult n m)
evenTimesOdd ZeroEven _     = ZeroEven
evenTimesOdd (NextEven e) o = oddPlusOdd (evenPlusOdd (evenTimesOdd e o) o) o

-- | Proves odd * even = even
oddTimesEven :: Odd n -> Even m -> Even (Mult n m)
oddTimesEven OneOdd e = e
oddTimesEven (NextOdd o) e = evenPlusEven (evenPlusEven (oddTimesEven o e) e) e
