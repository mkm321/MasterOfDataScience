p(X) :- not q(X), t(X).
q(X) :- not p(X), t(X).
t(a).
t(b).