import Data.Array
import Data.Foldable

listToArray :: [[a]] -> Array (Int, Int) a
listToArray xss = array ((0, 0), (length xss - 1, length (head xss) - 1)) $ do
    (rowNum, row) <- zip [0..] xss
    (colNum, e) <- zip [0..] row
    return ((rowNum, colNum), e)

findEntrance :: Array (Int, Int) Int -> (Int, Int)
findEntrance maze = [idx | idx <- indices maze, maze ! idx == 2] !! 0

mazeRunner :: [[Int]] -> [Char] -> [Char]
mazeRunner maze directions = solve directions entrance
    where m = listToArray maze
          entrance = findEntrance m
          solve :: [Char] -> (Int, Int) -> [Char]
          solve moves now @ (a, b) =
              if not (inRange (bounds m) now) || m ! now == 1 then "Dead"
              else if m ! now == 3 then "Finish"
              else if null moves then "Lost"
              else solve (tail moves) nextPosition
                   where move = head moves
                         nextPosition = case move of
                             'N' -> (a - 1, b)
                             'S' -> (a + 1, b)
                             'W' -> (a, b - 1)
                             'E' -> (a, b + 1)
