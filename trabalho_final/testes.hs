x = Var (0, "X")
y = Var (0, "Y")
z = Var (0, "Z")
teste = Var (0, "Bob")

--likes(bob, apple) --bob e apple são atomos (começam com letra minuscula)
likes = Predicate ("likes", [Atom "bob", Atom "apple"])
likes2 = Predicate ("likes", [Predicate ("wtf", [Var (0, "Ana")])])

main = do
    print $ unifyTerm teste likes2


db :: [Rule]
db = [
    (("likes", [Atom "bob", Atom "apple"]), []),
    (("likes", [Atom "alice", Atom "grape"]), []),
    (("likes", [Atom "charles", Var (0, "Y")]), [("yellow", [Var (0, "Y")])]),
    (("yellow", [Atom "banana"]), [])]

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

freshenTerm (Predicate (n, body)) =
    Predicate (name, mapear freshenTerm body)

--freshen tudo de atualizar o banco e tal...

unifyPred :: Predicate -> Predicate -> Maybe Substitution
unifyPred a b = Nothing

my_goal :: Predicate
my_goal = ("likes", [Atom "bob", Var (0, "X")]) --to perguntando oq q o bob gosta

resolve :: Predicate -> [Rule] -> [Substitution]
resolve goal rules =
    let rules' = fmap freshen rules in
    --usa monada List
    do 
        (pred, body) <- rules' 
        case unifyPred goal pred of
            Just t1 ->
                resolveBody rules' t1 body
            Nothing ->
                --zero respostas
                []
{-
resolve goal [] =
    []
resolve goal ((pred, body) : rules) =
    let rules' = fmap freshen rules in
        do
            (pred, body) <- rules'
            case unifyPred goal pred of
                Just u ->
                    resolveBody rules' u body
                Nothing ->
                    []
-}

substPred :: Substitution -> Predicate -> Predicate
substPred s e = e

compose :: Substitution -> Substitution -> Substitution
compose s e = s

resolveBody :: Substitution -> [Rule] -> [Predicate] -> [Substitution]
resolveBody t1 rules [] =
    return t1 --nao tem corpo, mas ja foi provado a parte de antes .... no bd -> likes(bob, apple), na pesquisa likes(bob, X), t1 = {X -> apple}
resolveBody t1 rules (p:ps) = do
    let rules' = fmap freshen rules in
        do
            t2 <- resolve (substPred t1 p) rules' 
            resolveBody (compose t2 t1) rules' ps


main :: IO ()
main = do
    --let db' = fmap (freshen . freshen) db
    --print db'
    let result = resolve my_goal db
    print result