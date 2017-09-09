import           Control.Monad.Except
import           Control.Monad.State
import           Data.Char            (isDigit, isLower, isUpper)
import qualified Data.Map.Strict      as M

data Molecule = Atom String
              | Many [Molecule]
              | Factor Molecule Int
              deriving Show

data Token = AtomTk String
           | Number Int
           | ParenL | ParenR
           | BrackL | BrackR
           | BraceL | BraceR
           deriving (Show, Eq)

lexer :: String -> Either String [Token]
lexer = sequence . lexer'
    where lexer' :: String -> [Either String Token]
          lexer' ""      = []
          lexer' (h : t) | isUpper h =
            let (h', t') = break (not . isLower) t
            in  Right (AtomTk $ h : h') : lexer' t'
          lexer' (h : t) | isDigit h =
            let (h', t') = break (not . isDigit) t
            in  Right (Number $ read (h: h')) : lexer' t'
          lexer' (h : t) = case h of
            '(' -> Right ParenL
            ')' -> Right ParenR
            '[' -> Right BrackL
            ']' -> Right BrackR
            '{' -> Right BraceL
            '}' -> Right BraceR
            _   -> Left "Invalid character"
            : lexer' t

parseSingle :: [Token] -> Either String (Molecule, [Token])
parseSingle (AtomTk atom : rest) = Right ((Atom atom), rest)
parseSingle (ParenL : rest) = do
    (m, rest') <- parseMany rest
    case rest' of
        (ParenR : rest'') -> Right (m, rest'')
        _                 -> throwError "parens don't match"
parseSingle (BrackL : rest) = do
    (m, rest') <- parseMany rest
    case rest' of
        (BrackR : rest'') -> Right (m, rest'')
        _                 -> throwError "brackets don't match"
parseSingle (BraceL : rest) = do
    (m, rest') <- parseMany rest
    case rest' of
        (BraceR : rest'') -> Right (m, rest'')
        _                 -> throwError "braces don't match"
parseSingle _ = Left "End"

parseFactor :: [Token] -> Either String (Molecule, [Token])
parseFactor tokens = do
    res @ (m, rest) <- parseSingle tokens
    case rest of
        (Number f) : rest' -> Right (Factor m f, rest')
        _                  -> Right res

parseMany :: [Token] -> Either String (Molecule, [Token])
parseMany tokens = do
    (m, rest) <- parseFactor tokens
    (ms, rest') <- parseMany' rest
    return (Many (m : ms), rest')
    where parseMany' :: [Token] -> Either String ([Molecule], [Token])
          parseMany' tokens' = case parseFactor tokens' of
            Left "End" -> Right ([], tokens')
            Right (m, rest) -> do
                (ms, rest') <- parseMany' rest
                return (m : ms, rest')
            Left err -> Left err

parse :: [Token] -> Either String Molecule
parse = (fst <$>) . parseMany

parseDebug :: String -> Either String Molecule
parseDebug s = do
    tokens <- lexer s
    parse tokens

eval :: Molecule -> M.Map String Int
eval (Atom atom)       = M.singleton atom 1
eval (Factor m factor) = fmap (* factor) (eval m)
eval (Many ms)         = M.unionsWith (+) (fmap eval ms)

parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = do
    tokens <- lexer formula
    molecule <- parse tokens
    let m = eval molecule
    return $ M.toList m
