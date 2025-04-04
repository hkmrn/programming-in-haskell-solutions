fac :: Int -> Int
fac 0 = 1
fac n
    | n >= 0 = n * fac (n-1)
    | otherwise = error "Factorial is not defined for negative numbers"