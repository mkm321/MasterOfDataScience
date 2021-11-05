father(john,sam).
mother(alice,sam).
gender_of(john,male).
gender_of(alice,female).
gender_of(sam,male).
parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).
child(X,Y) :- parent(Y,X).
father(john,bill).
mother(alice,bill).
gender_of(bill,male).
brother(X,Y) :- gender_of(X,male),
                father(F,X),
                father(F,Y),
                mother(M,X),
                mother(M,Y),
                X!=Y.
person(john).
person(sam).
person(alice).
person(bill).
-father(X,Y) :- gender_of(X,female),person(Y).
-father(X,Y) :- father(Z,Y),person(X),X!=Z.
person(bob).
-father(X,Y) :- not father(X,Y), person(X), person(Y).
%#show father/2.
%#show -father/2.