--ghci
-- :l um <- carrega o arquivo
-- :q <- sai do ghci

-- Uniao entre duas listas
uniao :: [a] -> [a] -> [a]
uniao [] x = 
    x
uniao (x:xs) y =
    x:uniao xs y
-- uniao [1,2,3] [4,5] =
--  1: uniao [2,3] [4,5] =
-- 1:2: uniao [3] [4,5] =
-- 1:2:3: uniao [] [4, 5] =
-- 1:2:3:[4,5]
-- 1:2:[3,4,5]
-- 1:[2,3,4,5]
-- [1,2,3,4,5]

-- se elemento pertence a lista
pertence :: Eq a => a -> [a] -> Bool
pertence a [] =
    False
pertence a (x:xs) =
    if a == x then
        True
    else
        pertence a xs

-- Intersecao entre duas listas
intersecao :: Eq a => [a] -> [a] -> [a]
intersecao a [] =
    []
intersecao [] a =
    []
intersecao (x:xs) y =
    if pertence x y then
        x : intersecao xs y
    else
        intersecao xs y

-- inverso de uma lista
inverso :: [a] -> [a]
inverso [] = []
inverso a =
    last (a) : inverso (init(a))

    -- n primeiros elementos
primeiros :: Int -> [a] -> [a]
primeiros 1 (x:xs) =
    [x]
primeiros n (x:xs) =
    x : primeiros (n-1) xs

-- n ultimos elementos
ultimos :: Int -> [a] -> [a]
ultimos n a =
    inverso(primeiros n (inverso(a)))
    