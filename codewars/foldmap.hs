import Data.Foldable (Foldable, foldMap)
import Data.Monoid

myToList :: Foldable t => t a -> [a]
myToList = foldMap return

newtype MinMaybe a = MinMaybe {get :: Maybe a}
instance Ord a => Monoid (MinMaybe a) where
    mempty = MinMaybe Nothing
    mappend (MinMaybe Nothing) a = a
    mappend a (MinMaybe Nothing) = a
    mappend a @ (MinMaybe (Just x))
            b @ (MinMaybe (Just y)) | x < y = a
                                    | otherwise = b

myMinimum :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum = get . foldMap (MinMaybe . Just)

myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f z = ($ z) . appEndo . foldMap (Endo . f)
