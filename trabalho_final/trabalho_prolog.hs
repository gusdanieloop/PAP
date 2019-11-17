import Data.Maybe (fromMaybe, fromJust)

--unificador de termos
unifyTerm :: Term -> Term -> Maybe Substitution
--caso (REFL)
-- duas variaveis que já são iguais, logo não precisa fazer nada
unifyTerm (Var x) (Var y) 
    | x == y =
        Just []
--caso (LEFT)
-- uma variavel do lado esquerdo que não aparece no lado direito, então posso tornar a variavel no que tem do lado direito
unifyTerm (Var x) e 
    | not (occursCheck x e) =
        Just [(x, e)] --substituir x por e { x |-> e }
--caso (RIGHT)
--uma variavel do lado direito que não aparece no lado esquerdo, então posso tornar a variavel no que tem do lado esquerdo
unifyTerm e (Var x) 
    | not (occursCheck x e) =
        Just [(x, e)] --substituir x por e { x |-> e }
--caso (PRED)
-- me ajuda
unifyTerm (Predicate p1) (Predicate p2) =
    unifyPred p1 p2

--caso (Atoms)
--da mesma forma que com variaveis iguais, nao precisa fazer nada pois ja são iguais
unifyTerm (Atom x) (Atom y) 
    | x == y = 
        Just []

--caso geral: nao é possivel unificar
unifyTerm _ _ = 
    Nothing

unifyPred :: Predicate -> Predicate -> Maybe Substitution
--socorro 2
unifyPred (n1, t1) (n2, t2) 
    | n1 /= n2 || length t1 /= length t2 =
        Nothing
    | otherwise =
        unifyBody t1 t2

unifyBody :: [Term] -> [Term] -> Maybe Substitution
--serio
unifyBody [] [] = Nothing

unifyBody (x:xs) (y:ys) = Just $ merge (fromJust (unifyTerm x y)) (fromJust (unifyBody xs ys))
-- TODO

merge :: [a] -> [a] -> [a]
merge [] xs = xs
merge (x:xs) y = x : merge xs y 

occursCheck :: Name -> Term -> Bool
occursCheck n (Var x) =
    n == x

occursCheck n (Predicate (_, t)) 
    | foldl (||) False (map (occursCheck n) t) =
        True                                    
    | otherwise =
        False

occursCheck _ _ =
    False

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = s1 ++ [(n, substTerm s1 t) | (n, t) <- s2, Var n /= t]

substTerm :: Substitution -> Term -> Term
--Atom é imutavel
substTerm _ (Atom n) = (Atom n)

--Var
substTerm s var@(Var name) =
    --se tiver alguma coisa pra substituir vai substituir, se nao vai deixar como tava
    fromMaybe var $ lookup name s

--Predicate
--substTerm s (Predicate p) =
--    Predicate $ substPred s p

--substPred :: Substitution -> Predicate -> Predicate

--substPred s (Predicate (n, t)) = 
--    Predicate (n, map (substTerm s) t)

data Term = 
    Atom String
    | Var Name
    | Predicate Predicate 
    deriving (Show, Eq)

type Name = 
    (Int, String)  

type Predicate = 
    (String, [Term])

type Rule = 
    (Predicate, [Predicate])

type Substitution = 
    [(Name, Term)]
