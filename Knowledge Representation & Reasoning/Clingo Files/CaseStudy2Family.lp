ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).
parent(alice,bob).
parent(bob,carol).
parent(dean,eric).
parent(bob,dean).