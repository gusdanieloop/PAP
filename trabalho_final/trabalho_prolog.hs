x = Var (0, "X")
y = Var (0, "Y")
z = Var (0, "Z")
teste = Var (0, "Bob")

--likes(bob, apple) --bob e apple são atomos (começam com letra minuscula)
likes = Predicate ("likes", [Atom "bob", Atom "apple"])
likes2 = Predicate ("likes", [Predicate ("wtf", [Var (0, "Ana")])])

main = do
    print $ unifyTerm teste likes2


    --unificador
unifyTerm :: Term -> Term -> Maybe Substitution

--caso (REFL)
--ja sao iguais, nao precisa colocar nada pra ficarem iguais
unifyTerm (Var x) (Var y) | x == y =
    Just []

--caso (LEFT)
unifyTerm (Var x) e | not (occursCheck x e) =
    Just [(x, e)] --substituir x por e { x |-> e }

--caso (RIGHT)
unifyTerm e (Var x) | not (occursCheck x e) =
    Just [(x, e)] --substituir x por e { x |-> e }

--caso (PRED)
--unifyTerm (Predicate (_, ))
unifyTerm (Predicate (n1, t1)) (Predicate (n2, t2)) | n1 /= n2 || length t1 /= length t2 =
    Nothing
                                                    | otherwise =
    Just [ zipWith (unifyTerm) t1 t2 ]

--caso (Atoms)
unifyTerm (Atom x) (Atom y) | x == y = 
    Just []
--TODO

--caso geral: nao é possivel unificar
unifyTerm _ _ = 
    Nothing

occursCheck :: Name -> Term -> Bool
occursCheck _ (Atom _) =
    False 
occursCheck n (Var x) | n == x =
    True
                      | otherwise = 
    False
occursCheck n (Predicate (_, t)) | foldl (||) False (map (occursCheck n) t) =
    True   
                                 | otherwise =
    False

data Term = Atom String
            | Var Name
            | Predicate Predicate
            deriving (Show, Eq)

type Name = (Int, String)

type Predicate = (String, [Term])

type Rule = (Predicate, [Predicate])

type Substitution = [(Name, Term)]
