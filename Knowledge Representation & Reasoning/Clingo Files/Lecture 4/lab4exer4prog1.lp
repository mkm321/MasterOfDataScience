adult(X) :- prof(X).
adult(X) | -adult(X) :- atom(X).
prof(X) | -prof(X) :- atom(X).
prof(alice).
-adult(bob).
atom(alice).
atom(bob).