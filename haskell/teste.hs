soma:: Int -> Int -> Int
soma a b = a + b

recsum:: Int -> Int
recsum n =
  if n == 0 then
    0
  else
    n + recsum (n - 1)


fibonacci:: Int -> Int
fibonacci 0 =
  0
fibonacci 1 =
  1
fibonacci n =
  fibonacci(n - 1) + fibonacci(n - 2)

-- fmap fibonacci [0..10] <-- cria um vetor com os fibonacci de 0 à 10

neg :: Bool -> Bool
neg False = True
neg True = False


-- 0 : 1 : 2 : [] --> [0,1,2]
-- 0 : 1 : 2 : [3, 4] --> [0,1,2,3,4] --> : concatena
-- cauda é tudo menos o 1 elemento
-- cabeça é tudo menos o ultimo elemento

primeiro :: [Int] -> Int
primeiro (x:y) =
    x

cauda :: [Int] -> [Int]
cauda (x:y) = 
    y

ult :: [Int] -> Int
ult (x:[]) =
    x
ult (x:y) =
    ult(y)

-- ultimo [] =
    -- error "nao pode"

--[1,2,3]
 -- [1, ]--> [2, ]-->[3, ]--> 0 / NULL


-- Nao informar o tipo com o treco de :: Tipo --> Tipo
-- haskell tenta adivinhar o tipo, mas o tipo de entrada tem que ser o tipo de saida
existeEmLista :: Eq a => a -> [a] -> Bool
existeEmLista a [] =
  False
existeEmLista a (x:xs) =
  if a == x then
    True
  else
    existeEmLista a xs

ultimo' :: [a] -> a
ultimo' (x:[]) =
  x
ultimo' (x:xs) =
  ultimo' xs

minhalista n =
  n : minhalista (n + 1)
-- gera infinitos numeros
-- take 10 (minhalista 0) =
-- take 10 (0 : minhalista 1) =
-- 0 : take 2 (minhalista 1) = 
-- 0 : take 2 (1: minhalista 2) =
-- 0:1:take 1 (minhalista 2) =
-- 0:1:take 1 (2: minhalista 3) =
-- 0:1:2 take 0 (minhalista 3) =
-- 0:1:2:[]
-- [0, 1, 2]

maiorElemento :: [int] -> Int
maiorElemento [] =
  error "nao pode"
maiorElemento (x:xs) =
  worker (x:xs) x
  where
    worker [] y =
      y
    worker (x:xs) y =
      let maior = if x > y then
                    x
                  else
                    y in
      worker xs maior