import Data.Char       (isAlpha, isDigit, ord, toLower)
import Data.Map.Strict (Map, fromList, (!))

presetColors :: Map String String
presetColors = fromList []

hexCharToInt :: Char -> Int
hexCharToInt c | isDigit c = ord c - ord '0'
               | isAlpha c = ord (toLower c) - ord 'a' + 10

hexToInt :: Char -> Char -> Int
hexToInt c1 c2 = hexCharToInt c1 * 16 + hexCharToInt c2

parseHtmlColor :: String -> Map Char Int
parseHtmlColor str = case str of
    '#' : r : g : b : [] -> parseRGB [r, r, g, g, b, b]
    '#' : rgb            -> parseRGB rgb
    preset               -> parseRGB $ presetColors ! map toLower preset
    where parseRGB :: String -> Map Char Int
          parseRGB (r1 : r2 : g1 : g2 : b1 : b2 : []) =
              fromList [('r', hexToInt r1 r2),
                        ('g', hexToInt g1 g2),
                        ('b', hexToInt b1 b2)]
