x = Var (0, "X")
y = Var (0, "Y")
z = Var (0, "Z")
teste = Var (0, "Bob")

--likes(bob, apple) --bob e apple são atomos (começam com letra minuscula)
likes = Predicate ("likes", [Atom "bob", Atom "apple"])
likes2 = Predicate ("likes", [Predicate ("wtf", [Var (0, "Ana")])])

main = do
    print $ unifyTerm teste likes2