-- 1er ejercicio

-- a

longitud :: [t] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs 

-- b

ultimo :: [t] -> t
ultimo [x] = x
ultimo (_:xs) = ultimo xs

-- c

principio :: [t] -> t
principio (x:_) = x 

-- d 

reverso :: [t] -> [t]
reverso [] = [] 
reverso (x:xs) = reverso xs ++ [x]

-- 2do ejercicio

-- a

pertenece :: Eq a => a -> [a] -> Bool
pertenece a [] = False
pertenece a (x:xs) = if (x == a) then True else pertenece a xs

-- b

todosIguales :: (Eq t) => [t] -> Bool
todosIguales [x] = True
todosIguales (x:xs) = if (x == xs!!0) then todosIguales xs else False

-- c

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [x] = True
todosDistintos (x:xs) = if (x /= xs!!0) then todosDistintos xs else False

-- d (se usa pertenece)

hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [x] = False
hayRepetidos (x:xs) = if (pertenece x xs) then True else hayRepetidos xs

-- e (se usa pertenece)

quitar :: (Eq t) => t -> [t] -> [t]
quitar a [] = []
quitar a (x:xs) = if (x == a) then xs else if (pertenece a xs) then quitar a (xs++[x]) else [x]++xs

-- f (se usa pertenece)

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos a [] = []
quitarTodos a (x:xs) = if (x == a) then (if (pertenece a xs) then quitarTodos a xs else xs) else if (pertenece a xs) then quitarTodos a (xs++[x]) else [x]++xs

-- g (se usa todosDistintos y pertenece)

eliminarRepetidos :: (Eq t) => [t] -> [t] 
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = if (todosDistintos ([x]++xs)) then [x]++xs else if (pertenece x xs) then eliminarRepetidos xs else eliminarRepetidos xs++[x]

-- h (se usa pertenece y eliminarRepetidos)

listasMismosElementos :: (Eq t) => [t] -> [t] -> Bool
listasMismosElementos [] _ = True
listasMismosElementos (x:xs) (y:ys) = if (pertenece x ([y]++ys)) then listasMismosElementos xs ([y]++ys) else False

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos xs ys = listasMismosElementos (eliminarRepetidos xs) (eliminarRepetidos ys)

-- i (se usa reverso)

capicua :: (Eq t) => [t] -> Bool
capicua xs = if (xs == (reverso xs)) then True else False

-- 3er ejerciciooooooo

-- a

sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- b

productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- c

maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:xs) = if (x > xs!!0) then maximo (xs++[x]) else maximo xs

minimo :: [Integer] -> Integer
minimo [x] = x
minimo (x:xs) = if (x < xs!!0) then minimo (xs++[x]) else minimo xs

-- d 

sumarN :: Integer -> [Integer] -> [Integer]
sumarN a [] = []
sumarN a (x:xs) = x + a : sumarN a xs 

-- e

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero [] = []
sumarElPrimero (x:xs) = sumarN x ([x]++xs)

-- f

sumarUltimo :: [Integer] -> [Integer]
sumarUltimo [] = []
sumarUltimo xs = sumarN (ultimo xs) xs

-- g

algunImpar :: [Integer] -> Bool
algunImpar [] = False
algunImpar (x:xs) = if (mod x 2 /= 0) then True else algunImpar (xs)

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) = if (algunImpar ([x]++xs)) then (if (mod x 2 /= 0) then pares xs else pares (xs++[x])) else ([x]++xs)

-- h

algunNoMultiplo :: Integer -> [Integer] -> Bool
algunNoMultiplo a [] = False
algunNoMultiplo a (x:xs) = if (mod x a /= 0) then True else algunNoMultiplo a (xs)

multiplos :: Integer -> [Integer] -> [Integer]
multiplos a [] = []
multiplos a (x:xs) = if (algunNoMultiplo a ([x]++xs)) then (if (mod x a /= 0) then multiplos a xs else multiplos a (xs++[x])) else ([x]++xs)

-- i

ordenarInverso :: [Integer] -> [Integer]
ordenarInverso [] = []
ordenarInverso xs = max : ordenarInverso (quitar max xs)  
  where max = maximo xs      

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar xs = min : ordenar (quitar min xs)  
  where min = minimo xs      

-- 4to ejerciciooooooooaooaooaoaoa

-- a

sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:y:xs)
    | x == ' ' && y == ' ' = sacarBlancosRepetidos (x:xs)
    | otherwise = x : sacarBlancosRepetidos (y:xs)

-- b

contarPalabras :: [Char] -> Integer
contarPalabras [] = 0
contarPalabras [x] = 1
contarPalabras (x:xs)
    | x == ' ' = 1 + contarPalabras xs
    | otherwise = contarPalabras (xs)

-- c

palabras :: [Char] -> [[Char]]
palabras [] = []
palabras [x] = [x]
palabras (x:y:xs)
    | y == ' ' = x ++ palabras xs
    | otherwise = palabras (xs)

-- d

