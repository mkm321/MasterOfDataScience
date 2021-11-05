fly(X) :- bird(X), not ab(X).
ab(X) :- penguin(X).
bird(X) :- penguin(X).
bird(tweety).
penguin(skippy).