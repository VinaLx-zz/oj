import Control.Monad.State
import Control.Monad.Writer
import Data.List            (break)
import Data.Map.Strict      (Map, fromList, (!))
import System.IO.Unsafe     (unsafePerformIO)

morseCodes :: Map String String
morseCodes = fromList [("....", "H"), (".", "E"), ("-.--", "Y"), (".---", "J"), ("..-", "U"), ("-..", "D")]

type Decoder = WriterT String (State String)

trim :: String -> String
trim = reverse . trim' . reverse . trim'
    where trim' = dropWhile (== ' ')

extract :: Decoder ()
extract = do
    input <- get
    let (word, rest) = break (== ' ') input
    tell $ morseCodes ! word
    put rest

decode :: Decoder ()
decode = do
    input <- get
    case input of
        []               -> return ()
        ' ' : ' ' : rest -> tell " " >> put rest >> decode
        ' ' : rest       -> put rest >> decode
        _                -> extract >> decode

decodeMorse :: String -> String
decodeMorse = fst . runState (execWriterT decode) . trim
