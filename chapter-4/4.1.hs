halve :: [a] -> ([a], [a])
halve xs = splitAt mid xs
    where mid = length xs `div` 2