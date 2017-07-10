import Data.List (foldl', intercalate)
data Range = Single Int
           | Twin Int Int
           | Between Int Int

instance Show Range where
    show (Single i)    = show i
    show (Twin i j)    = show i ++ "," ++ show j
    show (Between i j) = show i ++ "-" ++ show j

expand1 :: Range -> Range
expand1 (Single i)    = Twin i (i + 1)
expand1 (Twin i j)    = Between i (j + 1)
expand1 (Between i j) = Between i (j + 1)

solution :: [Int] -> String
solution = intercalate "," . reverse . map show . fst . foldl' step ([], 0)
    where step :: ([Range], Int) -> Int -> ([Range], Int)
          step ([], _) first = ([Single first], first)
          step (rs @ (r : rest), prev) cur =
              if cur - prev == 1
                  then (expand1 r : rest, cur)
                  else (Single cur : rs, cur)
