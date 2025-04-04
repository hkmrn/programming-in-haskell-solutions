sum' :: Num a => [a] -> a 
sum' [] = 0
sum' (n:ns) = n + sum ns

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
    | n > 0 = x : take' (n-1) xs 
    | otherwise = []

last' :: [a] -> a 
last' [x] = x 
last' (x:xs) = last' xs