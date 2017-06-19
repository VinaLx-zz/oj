import Data.List
import Data.Char

wordSearch :: String -> [String] -> Maybe [String]
wordSearch query seq =
    let lower = map toLower query
        result = (filter $ isInfixOf lower . map toLower) seq
    in if not $ null result
           then Just result
           else Nothing
