%% Consider the knowledge base Σ consisting of the following rules:
%% enrol(X,Y) ← not ab(d preRequsite(X,Y)). not ¬enrol(X,Y).

%% ¬enrol(X,Y) ← ¬completed preRequsite(X,Y).
%% ab(d preRequsite(X,Y)) ← not completed preRequsite(X,Y).
%% Now suppose we have the following facts:
%% enrol (alice , databases ). completed preRequsite(bob,ai).
%% What can we derive from Σ about Alice and Bob’s enrolling units?

enrol(alice,databases).
completed_pre(bob,ai).

student(alice).
unit(databases).
student(bob).
unit(ai).

enrol(X,Y) :- not ab(d_pre(X,Y)), not -enrol(X,Y), student(X),unit(Y).
-enrol(X,Y) :- -completed_pre(X,Y).
ab(d_pre(X,Y)) :- not completed_pre(X,Y), student(X),unit(Y).

#show -enrol/2.