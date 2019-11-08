main = do
    subprograma


subprograma :: IO ()
subprograma = do
    numeros <- lerNumeros
    print (converteParaArvore numeros)

lerNumeros :: IO [Int]
lerNumeros = do
    texto <- getLine
    let numero = read texto
    if numero == 0
        then do
            return []
        else do
            cauda <- lerNumeros
            return (numero : cauda)

data Tree a = Leaf a
            | Node (Tree a) ( Tree a)
            deriving Show

converteParaArvore :: [Int] -> Tree Int
converteParaArvore (x:[]) = Leaf x
converteParaArvore (x:xs) = Node (Leaf x) (converteParaArvore xs) 