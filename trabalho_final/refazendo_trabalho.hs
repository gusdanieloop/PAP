import System.Environment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

import Data.Maybe (fromMaybe, fromJust)

data Term = Atom String
          | Var Name
          | Predicate Predicate 
          deriving (Show, Eq)

type Name = (Int, String)  

type Predicate = (String, [Term])

type Rule = (Predicate, [Predicate])

type Substitution = [(Name, Term)]

{-
**********************************
**********PARSER******************
**********************************
-}

--name ::= lowercase alphanum*
name :: Parser String
name = do
    head <- lower
    tail <- many alphaNum
    return (head:tail)
-- atom ::= name | digit+
atom :: Parser Term
atom = do
    n <- name <|> many1 digit
    return (Atom n)
-- varialbe ::= uppercase alphanum*
variable :: Parser Term
variable = do
    head <- upper
    body <- many alphaNum
    return (Var (0, head:body))

-- arguments "(" (term ("," term)*)? ")"
arguments :: Parser [Term]
arguments = do
    char '('
    body <- sepBy term (char ',')
    char ')'
    return body
-- predicate ::= name arguments    
predicate :: Parser Term
predicate = do
    n <- name
    args <- arguments
    return (Predicate (n, args))  

-- rule ::= predicate (":-" predicate ("," predicate)*)? "."
rule :: Parser Rule
rule = do
    char ':'
    char '-'
    n <- predicate
    m <- sepBy predicate (char ',')
    char '.'
    return (n, m)

-- rules ::= rule + eof
rules :: Parser [Rule]
rules = do
    r <- sepBy rule (char '\n')
    fim <- eof
    return r
term :: Parser Term 
term = do
    try predicate <|> atom <|> variable 
{-
**************************************
**********END_PARSER******************
**************************************
-}

main :: IO ()

main = do
    args <- getArgs
    case args of
        [file] -> do
            content <- readFile file
            print (parse rule file content)

    {--putStrLn "Digite o tipo A: "
    a <- getLine
    putStrLn "Digite o tipo B: "
    b <- getLine
    --
    case (parse term "<stdin>" a, parse term "<stdin>" b) of
        (Right termo_a, Right termo_b) ->
            let mgu = unifyTerm termo_a termo_b in
                putStrLn ("Unificação de A com B: " ++ show mgu)
        x ->
            putStrLn "Erro Parsing"--}

{-
***********************************
************UNIFICADOR*************
***********************************
-}
unifyTerm :: Term -> Term -> Maybe Substitution

-- (REFL)
unifyTerm (Var a) (Var b) | a == b =
    Just []

-- (LEFT)
unifyTerm (Var a) b | not (occursCheck a b) =
    Just [(a, b)]

-- (RIGHT)
unifyTerm a (Var b) | not (occursCheck b a) =
    Just [(b, a)]

-- (PRED)
unifyTerm (Predicate p1) (Predicate p2) =
    unifyPred p1 p2

-- (ATOMS)
unifyTerm (Atom a) (Atom b) | a == b =
    Just []

-- caso geral
unifyTerm _ _ =
    Nothing

unifyPred :: Predicate -> Predicate -> Maybe Substitution

unifyPred (nome1, terms1) (nome2, terms2) | nome1 /= nome2 || length terms1 /= length terms2 =
    Nothing
                                          | otherwise =
    unifyBody terms1 terms2

unifyBody :: [Term] -> [Term] -> Maybe Substitution

unifyBody [] [] = 
    Nothing

unifyBody (x:[]) (y:[]) =
    unifyTerm x y
    
unifyBody (x:xs) (y:ys) = do
    change1 <- unifyTerm x y
    change2 <- unifyBody (mapear (substTerm change1) xs) (mapear (substTerm change1) ys)
    Just (compose change1 change2)

occursCheck :: Name -> Term -> Bool
occursCheck name (Var name2) =
    name == name2

occursCheck name (Predicate (_, terms)) = 
    foldl (||) False (mapear (occursCheck name) terms)

occursCheck _ _ =
    False

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = s1 ++ [(n, substTerm s1 t) | (n, t) <- s2, Var n /= t]

substTerm :: Substitution -> Term -> Term

substTerm _ (Atom a) = 
    (Atom a)

substTerm s var@(Var name) =
    fromMaybe var $ lookup name s

substTerm s (Predicate (x, terms)) =
    Predicate (x, mapear (substTerm s) terms)

{-
***************************************
************END_UNIFICADOR*************
***************************************
-}

concatenar :: [a] -> [a] -> [a]
concatenar = (++)

mapear :: (a -> b) -> [a] -> [b]
mapear = fmap