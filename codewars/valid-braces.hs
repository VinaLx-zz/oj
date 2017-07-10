import Data.List (elemIndex, foldl')

match :: Char -> Char -> Bool
match l r = elemIndex l "([{" == elemIndex r ")]}"

validBraces :: String -> Bool
validBraces str = let (l, b) = foldl' step ([], True) str in null l && b
    where step :: ([Char], Bool) -> Char -> ([Char], Bool)
          step (_, False) _ = ([], False)
          step ([], _) c | c `elem` ")]}" = ([], False)
                         | otherwise = ([c], True)
          step (l @ (a : xs), _) c | c `elem` "([{" = (c : l, True)
                                   | a `match` c = (xs, True)
                                   | otherwise = ([], False)
