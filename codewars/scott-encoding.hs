{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ScottEncoding where

import Prelude hiding
    ( concat
    , curry
    , foldl
    , foldr
    , fst
    , length
    , map
    , null
    , snd
    , take
    , uncurry
    , zip
    , (++)
    )

newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a,b)
toPair p = (fst p, snd p)

fromPair :: (a,b) -> SPair a b
fromPair (a, b) = SPair $ \f -> f a b

fst :: SPair a b -> a
fst (SPair f) = f const

snd :: SPair a b -> b
snd (SPair f) = f (flip const)

swap :: SPair a b -> SPair b a
swap (SPair f) = SPair (f . flip)

curry :: (SPair a b -> c) -> (a -> b -> c)
curry f a b = f $ fromPair (a, b)

uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f (SPair f') = f' f

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }

toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe m) = m Nothing Just

fromMaybe :: Maybe a -> SMaybe a
fromMaybe Nothing  = SMaybe const
fromMaybe (Just a) = SMaybe $ \_ f -> f a

isJust :: SMaybe a -> Bool
isJust (SMaybe f) = f False (const True)

isNothing :: SMaybe a -> Bool
isNothing = not . isJust

unsafeGet :: SMaybe a -> a
unsafeGet (SMaybe f) = f undefined id

catMaybes :: SList (SMaybe a) -> SList a
catMaybes l = map unsafeGet $ filterS isJust l

newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }

toEither :: SEither a b -> Either a b
toEither (SEither f) = f Left Right

fromEither :: Either a b -> SEither a b
fromEither (Left a)  = SEither $ \f _ -> f a
fromEither (Right b) = SEither $ \_ f -> f b

isLeft :: SEither a b -> Bool
isLeft (SEither f) = f (const True) (const False)

unsafeLeft :: SEither a b -> a
unsafeLeft (SEither f) = f id undefined

isRight :: SEither a b -> Bool
isRight = not . isLeft

unsafeRight :: SEither a b -> b
unsafeRight (SEither f) = f undefined id

partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition l = fromPair $ (,)
    (map unsafeLeft $ filterS isLeft l)
    (map unsafeRight $ filterS isRight l)

newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }

toList :: SList a -> [a]
toList (SList f) = f [] $ \a t -> a : toList t

fromList :: [a] -> SList a
fromList []       = SList const
fromList (a : as) = SList $ \_ f -> f a $ fromList as

cons :: a -> SList a -> SList a
cons a l = SList $ \_ f -> f a l

nilS :: SList a
nilS = fromList []

concat :: SList a -> SList a -> SList a
concat (SList fl) rhs = fl rhs $ \a l -> cons a (concat l rhs)

null :: SList a -> Bool
null (SList f) = f True $ \_ _ -> False

length :: SList a -> Int
length = foldl (+) 0 . map (const 1)

map :: (a -> b) -> SList a -> SList b
map f = foldr (cons . f) (fromList [])

zip :: SList a -> SList b -> SList (SPair a b)
zip (SList fa) (SList fb) = fa nilS $ \a as ->
    fb nilS $ \b bs -> cons (fromPair (a, b)) (zip as bs)

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f z l = foldr (\a acc b -> acc (f b a)) id l z

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr step z (SList f) = f z $ \a l -> step a (foldr step z l)

filterS :: (a -> Bool) -> SList a -> SList a
filterS p (SList f) = f nilS $ \a l ->
    if p a then cons a $ filterS p l else filterS p l

take :: Int -> SList a -> SList a
take n (SList f) = f nilS $ \a l ->
    if n <= 0 then nilS else cons a $ take (n - 1) l
