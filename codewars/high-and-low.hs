import Data.Foldable

strToInts :: String -> [Int]
strToInts = map read . words

intsToStr :: (Int, Int) -> String
intsToStr (a, b) = show a ++ " " ++ show b

step :: (Int, Int) -> Int -> (Int, Int)
step (a, b) c | c > a = (c, b)
              | c < b = (a, c)
              | otherwise = (a, b)

highAndLowImpl :: [Int] -> (Int, Int)
highAndLowImpl (a : xs) = foldl' step (a, a) xs

highAndLow :: String -> String
highAndLow = intsToStr . highAndLowImpl . strToInts
