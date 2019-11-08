dobrar_dir :: (a->b->b) -> b -> [a] -> b
dobrar_dir funcao acumulador [] = acumulador
dobrar_dir funcao acumulador (x:xs) = 
    funcao x (dobrar_dir funcao acumulador xs)

-- dobrar_dir (+) 0 [1,2,3] = 1 + (2 + (3 + 0))
-- dobrar_dir (:) [3] [1, 2] = 1 : (2: [3])

dobrar_esq :: (b -> a -> b) -> b -> [a] -> b
dobrar_esq funcao acumulador [] = acumulador
dobrar_esq funcao acumulador (x:xs) =
    dobrar_esq funcao (funcao acumulador x) xs

-- dobrar_esq (+) 0 [1,2,3] = ((0+1)+2)+3
