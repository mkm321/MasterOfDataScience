adult(X) :- prof(X).
-prof(X) :- -adult(X).
prof(alice).
-adult(bob).
