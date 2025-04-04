and' :: [Bool] -> Bool 
and' [] = True
and' (b:bs) = b && and' bs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

(!!*) :: [a] -> Int -> a 
(x:_) !!* 0 = x 
(_:xs) !!* n = xs !!* (n-1)
_ !!* _ = error "Index out of bounds"

elem' :: Eq a => a -> [a] -> Bool 
elem' _ [] = False
elem' x (y:ys)
    | x == y = True
    | otherwise = elem' x ys