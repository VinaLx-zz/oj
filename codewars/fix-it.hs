module Fixit where

reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' _ []       = []
reverse' f (x : xs) = f xs ++ [x]

foldr' :: ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
foldr' _ _ z []          = z
foldr' f step z (x : xs) = step x (f step z xs)
