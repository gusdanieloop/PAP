likes(alice, pear).
likes(alice, orange).
likes(bob, pear).
likes(bob, X) :- yellow(X).
likes(charlie, X) :- likes(alice, X), likes(bob, X).
