import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concatMap (addParity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int . removeParity) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = tail

getParity :: [Bit] -> Int 
getParity bs = sum bs `mod` 2

addParity :: [Bit] -> [Bit]
addParity bs = bs ++ [getParity bs]

removeParity :: [Bit] -> [Bit]
removeParity bs
    | realParity == givenParity = init bs 
    | otherwise = error "Parity error"
    where 
        realParity = getParity (init bs)
        givenParity = last bs