father(john,sam). 
mother(alice,sam).
gender_of(john,male).
gender_of(alice,female).
gender_of(sam,male).
parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).
child(X,Y) :- parent(Y,X).	

