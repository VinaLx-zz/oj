import Data.Char

isPalindrom :: String -> Bool
isPalindrom str = reverse upper == upper
    where upper = map toUpper str
