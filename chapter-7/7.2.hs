all' :: (a -> Bool) -> [a] -> Bool 
all' p = foldr ((&&) . p) True

any' :: (a -> Bool) -> [a] -> Bool 
any' p = foldr((||) . p) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs 
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p (x:xs)
    | p x = dropWhile' p xs 
    | otherwise = x:xs