-- recursion

factorial :: Integer -> Integer
factorial n
    | n == 0 = 1
    | n > 0 = n * factorial (n-1)

-- fibonacci 1

fibonacci :: Integer -> Integer
fibonacci n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fibonacci (n - 1) + fibonacci (n-2) 

-- 2

parteEntera :: Float -> Integer
parteEntera n = floor n

-- 3

-- esDivisible :: Integer -> Integer -> Bool
-- esDivisible x y
--     | x / y /= 0 = False
--     | otherwise = True

-- 4

-- sumaImpares :: Integer -> Integer
-- sumaImpares n
--     |
--     |

-- 5

medioFact :: Integer -> Integer
medioFact n
    | n < 0 = (-n)
    | n == 0 = 1
    | otherwise = n * medioFact (n-2)

-- 6

sumaDigitos :: Integer -> Integer
sumaDigitos n 
    | n == 0 = 0
    | n > 0 = mod n 10 + sumaDigitos (div n 10)


-- 7

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n 
    | n <= 0 = False
    | n < 10 = True
    | otherwise = mod n 10 == mod (div n 10) 10 && todosDigitosIguales (div n 10)
    
-- 22 - 2 == 2 = 2
-- 25 - 5 == 2 /= 

-- 8

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito x i 
    | x < 10 = x
    | i == 1 = mod x 10
    | otherwise = iesimoDigito (div x 10) (i-1)

cantDigitos :: Integer -> Integer
cantDigitos n
    | n<10 = 1
    | otherwise = 1 + cantDigitos (div n 10)  

-- 9

esCapicua :: Integer -> Bool
esCapicua x 
    | cantDigitos x == 1 = True
    | otherwise = iesimoDigito x (cantDigitos x) == iesimoDigito x 1 && esCapicua (reducirNum x)

reducirNum :: Integer -> Integer
reducirNum n = div (mod n (10^(cantDigitos n - 1))) 10