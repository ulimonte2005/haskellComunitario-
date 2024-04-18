maximo :: Integer -> Integer -> Integer
maximo n m | n>=m = n 
           | otherwise = m

maximo3 :: (Integer -> Integer) -> Integer 
maximo3 n = n 3 2
