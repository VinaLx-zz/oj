module CanYouGetTheLoop where

data Node a
instance Eq a => Eq (Node a) where
    (==) = undefined

next :: Node a -> Node a
next = undefined

findStart :: Eq a => Node a -> Node a
findStart n = findStart' (next n) (next (next n))
    where findStart' slow fast =
            if slow == fast
                then slow
                else findStart' (next slow) (next (next fast))

countLoop :: Eq a => Node a -> Int
countLoop n = countLoop' 1 (next n) (next (next n))
    where countLoop' c slow fast = if slow == fast
            then c else countLoop' (succ c) (next slow) (next (next fast))

loopSize :: Eq a => Node a -> Int
loopSize n = countLoop $ findStart n

