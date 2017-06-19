import Data.Bits
import Data.Foldable

findUnique :: [Int] -> Int
findUnique = foldl' xor 0
