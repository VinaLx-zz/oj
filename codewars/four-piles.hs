
calculateX :: Int -> Int -> Maybe Int
calculateX n y =
    let a = fromIntegral $ n * y :: Double
        b = (fromIntegral y + 1) ** 2
        x = a / b
     in if x - fromIntegral (round x) < 0.0001 && x > fromIntegral y
           then Just (round x)
           else Nothing

fourPiles :: Int -> Int -> [Int]
fourPiles n y =
    case calculateX n y of
        Just x ->
            [y + x, x - y, y * x, round (fromIntegral x / fromIntegral y :: Double)]
        Nothing -> []
