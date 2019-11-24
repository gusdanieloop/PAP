import System.Environment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad (void)
import Data.Maybe (fromMaybe, fromJust)

data Term = Atom String
          | Var Name
          | Predicate Predicate 
          deriving (Show, Eq)

type Name = (Int, String)  

type Predicate = (String, [Term])

type Rule = (Predicate, [Predicate])

type Substitution = [(Name, Term)]

-- Parser

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

rules :: Parser [Rule]
rules = do
   -- vai ler os espaços e vai ignorar isso, entao vai pegar todas as rules 
   whitespace *> many1 rule

rule :: Parser Rule
rule = do
    head <- predicate
    body <- end <|> whitespace *> string ":-" *> whitespace *> (predicate `sepBy` (char ',' *> whitespace)) <* end
    return (head, body)
    where 
        end = [] <$ char '.' <* whitespace

predicate :: Parser Predicate
predicate = do
    n <- name
    args <- arguments
    return (n, args)

name :: Parser String
name = do
    head <- lower
    tail <- many alphaNum
    return $ head:tail

arguments :: Parser [Term]
arguments = do
    body <- char '(' *> (term `sepBy1` (char ',' *> whitespace)) <* char ')'
    return body

term :: Parser Term
term = do
    try (Predicate <$> predicate) <|> atom <|> variable

atom :: Parser Term
atom = do
    n <- name <|> many1 digit
    return $ Atom n

variable :: Parser Term
variable = do
    head <- upper
    tail <- many alphaNum
    return $ Var(0, head:tail)

-- Substituições

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = s1 ++ [(n, substTerm s1 t) | (n, t) <- s2, Var n /= t]

substTerm :: Substitution -> Term -> Term
substTerm s var@(Var name) = fromMaybe var $ lookup name s
substTerm s (Predicate p) = Predicate $ substPred s p
substTerm _ atom = atom

substPred :: Substitution -> Predicate -> Predicate
substPred s (x, terms) = (x, mapear (substTerm s) terms)

-- unificador

unifyTerm :: Term -> Term -> Maybe Substitution
unifyTerm (Var a) (Var b) | a == b = Just []
unifyTerm (Var a) b | not $ occursCheck a b = Just [(a, b)]
unifyTerm a (Var b) | not $ occursCheck b a = Just [(b, a)]
unifyTerm  (Predicate p1) (Predicate p2) = unifyPred p1 p2
unifyTerm (Atom a) (Atom b) | a == b = Just []
unifyTerm _ _ = Nothing

unifyPred :: Predicate -> Predicate -> Maybe Substitution
unifyPred (n1, t1) (n2, t2) | n1 /= n2 || length t1 /= length t2 = Nothing
                            | otherwise = unifyBody t1 t2

unifyBody :: [Term] -> [Term] -> Maybe Substitution
unifyBody [] [] = Nothing
unifyBody (x:[]) (y:[]) = unifyTerm x y
unifyBody (x:xs) (y:ys) = do
    change1 <- unifyTerm x y
    change2 <- unifyBody (mapear (substTerm change1) xs) (mapear (substTerm change1) ys)
    Just (compose change1 change2)

-- verificador

occursCheck :: Name -> Term -> Bool
occursCheck name (Var name2) = name == name2
occursCheck name (Predicate (_, terms)) = foldl (||) False (mapear (occursCheck name) terms)
occursCheck _ _ = False

-- update variavel

freshen :: Rule -> Rule
freshen (pred, body) = (freshenPred pred, mapear freshenPred body)

freshenPred :: Predicate -> Predicate
freshenPred (name, terms) = (name, mapear freshenTerm terms)

freshenTerm :: Term -> Term
freshenTerm (Var (i, s)) = Var (i + 1, s)
freshenTerm (Predicate (name, body)) = Predicate (name, mapear freshenTerm body)
freshenTerm atom = atom

-- resolver o problema

resolve :: Predicate -> [Rule] -> [Substitution]
resolve goal rules = 
    let rules' = mapear freshen rules in
        do
            (pred, body) <- rules'
            case unifyPred goal pred of
                Just t1 -> resolveBody t1 rules' body
                Nothing -> []

resolveBody :: Substitution -> [Rule] -> [Predicate] -> [Substitution]
resolveBody t1 rules [] = return t1
resolveBody t1 rules (p:ps) = do
    let rules' = mapear freshen rules in
        do
            t2 <- resolve (Predicate <$> (substTerm t1 p)) rules'
            resolveBody (compose t2 t1) rules' ps

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            content <- readFile file
            print (parse rules file content)
    

-- funções auxiliares

mapear :: (a -> b) -> [a] -> [b]
mapear = fmap

concatenar :: [a] -> [a] -> [a]
concatenar = (++) 