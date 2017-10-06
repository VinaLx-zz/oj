{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module PolyvariadicFunctions where

class AddVariadic a where
    accum :: Int -> a

instance AddVariadic Int where
    accum = id

instance (Integral a, AddVariadic b) => AddVariadic (a -> b) where
    accum i a = accum (i + fromIntegral a)

-- `polyAdd` sums its arguments, all `Int`s.
polyAdd :: AddVariadic a => a
polyAdd = accum 0

class ListVariadic a b | b -> a where
    accumL :: [a] -> b

instance ListVariadic a [a] where
    accumL = id

instance ListVariadic a b => ListVariadic a (a -> b) where
    accumL as a = accumL (as ++ [a])

polyList :: ListVariadic a b => b
polyList = accumL []

class WordVariadic a where
    accumW :: [String] -> a

instance WordVariadic String where
    accumW = unwords

instance WordVariadic a => WordVariadic (String -> a) where
    accumW ss s = accumW (ss ++ [s])

-- `polyWords` turns its arguments into a spaced string.
polyWords :: WordVariadic a => a
polyWords = accumW []
