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

term :: Parser Term 
term = do
    try predicate <|> atom <|> variable 
{-
**************************************
**********END_PARSER******************
**************************************
-}

db :: [Rule]
db = [
    (("likes", [Atom "alice", Atom "pear"]), []),
    (("likes", [Atom "alice", Atom "orange"]), []),
    (("likes", [Atom "bob", Atom "pear"]), []),
    (("likes", [Atom "bob", Var (0, "X")]), [("yellow", [Var (0, "X")])]),
    (("likes", [Atom "charlie", Var (0, "X")]), [("likes", [Atom "alice", Var (0, "X")]), ("likes", [Atom "bob", Var (0, "X")])]),
    (("yellow", [Atom "banana"]), [])]

my_goal :: Predicate
my_goal = ("likes", [Atom "charlie", Var (0, "X")])

main :: IO ()

main = do
    {-
    args <- getArgs
    case args of
        [file] -> do
            content <- readFile file
            print (parse rules file content)
    -}
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
    let result = resolve my_goal db
    print result
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

{-
***************************************
***********SUBST_VAR_NUMBER************
***************************************
-}

freshen :: Rule -> Rule
freshen ((name, terms), body) =
    ((name, mapear freshenTerm terms), body)

freshen (pred, body) =
    (freshenPred pred, mapear freshenPred body)

freshenPred :: Predicate -> Predicate
freshenPred (name, terms) =
    (name, mapear freshenTerm terms)
        
freshenTerm :: Term -> Term
freshenTerm (Atom s) = 
    Atom s

freshenTerm (Var (i, s)) =
    Var (i + 1, s)

freshenTerm (Predicate (name, body)) =
    Predicate (name, mapear freshenTerm body)

{-
***************************************
*********SUBST_VAR_NUMBER_END**********
***************************************
-}

{-
***************************************
***********RESOLVER_REQUEST************
***************************************
-}
resolve :: Predicate -> [Rule] -> [Substitution]
resolve goal rules =
    let rules' = fmap freshen rules in
    --usa monada List
    do 
        (pred, body) <- rules' 
        case unifyPred goal pred of
            Just t1 ->
                resolveBody t1 rules' body
            Nothing ->
                --zero respostas
                []

resolveBody :: Substitution -> [Rule] -> [Predicate] -> [Substitution]
resolveBody t1 rules [] =
    return t1 --nao tem corpo, mas ja foi provado a parte de antes .... no bd -> likes(bob, apple), na pesquisa likes(bob, X), t1 = {X -> apple}
resolveBody t1 rules (p:ps) = do
    let rules' = fmap freshen rules in
        do
            t2 <- resolve (substTerm t1 p) rules' 
            resolveBody (compose t2 t1) rules' ps
{-
***************************************
*********RESOLVER_REQUEST_END**********
***************************************
-}

concatenar :: [a] -> [a] -> [a]
concatenar = (++)

mapear :: (a -> b) -> [a] -> [b]
mapear = fmap