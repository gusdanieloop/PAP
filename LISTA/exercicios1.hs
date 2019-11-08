-- 1.	Escreva uma função que retorne a concatenação entre duas listas.
concatenacao :: [a] -> [a] -> [a]
concatenacao [] a = a
concatenacao (x:xs) a = x: concatenacao xs a
--2.	Escreva uma função que retorne se um elemento pertence a uma lista.
pertence :: Eq a => a -> [a] -> Bool
pertence a [] = False
pertence a (x:xs) =
    if a == x then
        True
    else
        pertence a xs
--3.	Escreva uma função que retorne a interseção entre duas listas.
intersecao :: Eq a => [a] -> [a] -> [a]
intersecao [] a = []
intersecao (x:xs) a =
    if pertence x a then
        x:intersecao xs a
    else
        intersecao xs a
--4.	Escreva uma função que retorne o inverso de uma lista.
inverso :: [a] -> [a]
inverso (x:[]) = [x]
inverso (x:xs) =
    concatenacao (inverso xs) [x] 
--5.	Escreva uma função que retorna os n primeiros elementos de uma lista.
primeiros :: Int -> [a] -> [a]
primeiros 1 (x:xs) = [x]
primeiros n (x:xs) =
    x: primeiros (n-1) xs
--6.	Escreva uma função que retorne os n últimos elementos de uma lista.
ultimos :: Int -> [a] -> [a]
ultimos n a =
    inverso (primeiros n (inverso a))
--7.	Escreva uma função que converta um número binário, representado como uma string, em um número inteiro:
binParaInt :: String -> Int
binParaInt (x:[])   | x == '1' = 1
                    | otherwise = 0
binParaInt (x:xs)   | x == '1' = 2 ^ (length xs) + binParaInt xs
                    | otherwise = binParaInt xs
--8.	Escreva uma função que converta um número inteiro para um número binário, representado como uma string:
intParaBin :: Int -> String
intParaBin x | x < 2 = show x
             | otherwise = intParaBin (x`div`2) ++ show (x`mod`2)
--9.	Escreva uma função que retorna o menor valor de uma lista:
menorValor :: Ord a => [a] -> a
menorValor (x:[]) = x
menorValor (x:xs) | x < menorValor xs = x
                  | otherwise = menorValor xs
--10.	Escreva uma função que receba uma lista e um elemento e retorne a lista sem a primeira ocorrência desse elemento:
removerPrimeiro :: Eq a => [a] -> a -> [a]
removerPrimeiro (x:xs) y | x == y = xs
                         | otherwise = x: removerPrimeiro xs y
--11.	Escreva uma função, utilizando os exercícios anteriores, que ordene os elementos de uma lista:
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar xs = menorValor xs: ordenar (removerPrimeiro xs (menorValor xs))

--12.   Escreva uma função que dobre uma lista pela direita
dobrar_dir :: (a -> b -> b) -> b -> [a] -> b
dobrar_dir funcao acumulador [] = acumulador
dobrar_dir funcao acumulador (x:xs) = funcao x (dobrar_dir funcao acumulador xs)  

--13.   Escreva uma função que dobre uma lista pela esquerda
dobrar_esq :: (b -> a -> b) -> b -> [a] -> b
dobrar_esq funcao acumulador [] = acumulador
dobrar_esq funcao acumulador (x:xs) = dobrar_esq funcao (funcao acumulador x) xs 

--14. Escreva uma funcao que filtre uma lista, retornando os elementos que satisfazem um predicado
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar verificador [] = []
filtrar verificador (x:xs) | verificador x == True = x : filtrar verificador xs
                           | otherwise = filtrar verificador xs

--filtrar (\x -> x `mod` 2 == 0) [1,-2,3,4]
--filtrar ((==) 0 . flip mod 2) [1,-2,3,4]