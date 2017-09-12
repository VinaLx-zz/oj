module Postfix where

begin :: ([Int] -> r) -> r
begin = ($ [])

push :: [Int] -> Int -> ([Int] -> r) -> r
push xs x f = f (x : xs)

add :: [Int] -> ([Int] -> r) -> r
add (x : y : xs) f = f $ (x + y) : xs

end :: [Int] -> Int
end = head
