xor :: Bool -> Bool -> Bool
xor a b = not a && b || a && not b
gimme :: Ord a => (a, a, a) -> Int
gimme (a, b, c) | (a < b) `xor` (c < b) = 1
                | (b < a) `xor` (c < a) = 0
                | (a < c) `xor` (b < c) = 2
