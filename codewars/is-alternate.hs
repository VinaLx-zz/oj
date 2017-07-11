isVowel :: Char -> Bool
isVowel c | c `elem` "aeiou" = True
          | otherwise = False

isAltV :: [Char] -> Bool
isAltV []      = True
isAltV (h : t) = isVowel h && isAltC t

isAltC :: [Char] -> Bool
isAltC []      = True
isAltC (h : t) = not (isVowel h) && isAltV t

isAlt :: [Char] -> Bool
isAlt (h : t) | isVowel h = isAltC t
              | otherwise = isAltV t
isAlt [] = True
