import Text.ParserCombinators.Parsec

--main :: IO()
--main = do
    --args <- getArgs
--    putStrLn "Hello there!"
--    name <- getLine
--    case name of
--        "quit" -> return()
--        -> putStrLn $ "Hello" ++name 
main :: IO ()
main = do
    print $ multiply [1, 3, 7]

repl :: IO ()
repl = do
    putStrLn "?- "
    line <- getLine
    --testar se a linha é um predicado válido em prolog
    putStrLn ("Você digitou: " ++ line)
    repl


multiply :: [Int] -> [Int]
multiply lista = do
    x <- lista
    y <- [2, 10]
    if x * y > 10
        then return (x * y)
    else []


foo :: Maybe Int
foo = Just 10

bar :: Maybe Int
bar = Nothing --Just 20

soma :: Maybe Int -> Maybe Int -> Maybe Int
soma x y = do
    valor_x <- x
    valor_y <- y
    return (valor_x + valor_y)