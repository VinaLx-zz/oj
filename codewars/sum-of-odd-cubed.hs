oddCubed :: [Int] -> Int
oddCubed = sum . map (^ 3) . filter odd
