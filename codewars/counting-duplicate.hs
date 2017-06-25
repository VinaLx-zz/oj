import           Data.Array
import           Data.Char
import           Data.Foldable
import qualified Data.Set      as S

type SA = (S.Set Char, Array Char Int)

duplicateCount :: String -> Int
duplicateCount = sum . snd
    . foldl' step (S.empty, listArray (chr 0, chr 127) (repeat 0))
    . (fmap toUpper)
    where step :: SA -> Char -> SA
          step (s, a) now = if now `S.member` s
              then (s, a // [(now, 1)])
              else (now `S.insert` s, a)
