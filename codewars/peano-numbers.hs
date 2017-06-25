import Prelude hiding
    (Double, Float, Int, Integer, Num, Rational, Word, compare, div, even, odd)

data Peano = Zero | Succ Peano deriving (Eq, Show)

add, sub, mul, div :: Peano -> Peano -> Peano
-- Addition
add x y = case y of
    Zero    -> x
    Succ y' -> Succ $ add x y'

-- Subtraction
sub x Zero            = x
sub Zero _            = error "negative number"
sub (Succ x) (Succ y) = sub x y

-- Multiplication
mul Zero _        = Zero
mul _ Zero        = Zero
mul x (Succ Zero) = x
mul x (Succ y)    = add x (mul x y)

-- Integer division
div _ Zero = error "divide by 0"
div a b | compare a b == LT = Zero
        | otherwise = Succ $ div (sub a b) b

even, odd :: Peano -> Bool
-- Even
even Zero     = True
even (Succ x) = odd x
-- Odd
odd Zero     = False
odd (Succ x) = even x

compare :: Peano -> Peano -> Ordering
compare Zero Zero         = EQ
compare Zero _            = LT
compare _ Zero            = GT
compare (Succ x) (Succ y) = compare x y
