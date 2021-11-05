%% 1. Consider the following program Π:
%% r1: q(a,b).
%% r2: r(a,c).
%% r3: p(X,Y) ← q(X,Y),not ab(d p(X,Y)), 
%% r4: ab(d p(X,Y)) ← not ¬r(X,Y),
%% r5: ¬r(X,Y) ← not r(X,Y).
%% Translate this program into clingo syntax, and run it under clingo. 
%% Note that in order to make your clingo program be safe,
%% you will need to modify some rules in the original program. 
%% Then by removing rule r5, run the program again. 
%% Compare the result with the previous one, what do you observe?

q(a,b).
r(a,c).
atom(a).
atom(b).
atom(c).
p(X,Y) :- q(X,Y), not ab(d_p(X,Y)).
ab(d_p(X,Y)) :- not -r(X,Y), atom(X), atom(Y).
-r(X,Y) :- not r(X,Y), atom(X), atom(Y).