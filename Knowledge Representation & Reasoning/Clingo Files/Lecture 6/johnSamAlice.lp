parent(alice,sam).
parent(john,sam).
atom(john,sam).
atom(alice,sam).
%%-absent(alice,sam).
-cares(john,C) :- parent(john,C).
cares(X,Y) :- parent(X,Y),
                not ab(d_cares(X,Y)),
                not -cares(X,Y).
ab(d_cares(X,Y)) :- not -absent(X,Y), atom(X,Y).