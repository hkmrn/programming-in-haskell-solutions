fxs :: (a -> b) -> (a -> Bool) -> [a] -> [b]
fxs f p = map f . filter p