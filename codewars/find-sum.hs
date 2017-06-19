merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (a : xs) (b : ys)
    | a < b = a : merge xs (b : ys)
    | a > b = b : merge (a : xs) ys
    | a == b = a : merge xs ys

findSum :: Int -> Int
findSum n = sum $ takeWhile (<= n) $ merge [3, 6 ..] [5, 10 ..]
