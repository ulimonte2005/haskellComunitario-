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

-- g 

-- eliminarRepetidos :: (Eq t) => [t] -> [t] 
-- eliminarRepetidos [] = []
-- eliminarRepetidos [x] = [x]
-- eliminarRepetidos (x:xs) = if (pertenece x xs) then eliminarRepetidos xs else xs

-- h

-- falta este xd

-- i

capicua :: (Eq t) => [t] -> Bool
capicua xs = if (xs == (reverso xs)) then True else False
