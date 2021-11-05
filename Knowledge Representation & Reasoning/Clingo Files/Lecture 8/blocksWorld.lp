%% Dynamic Blocks World

%% System Decription SD:

%% Causal law:
%%    put(B,L) causes on(B,L)
%% State constraints:
%%    -on(B,L2) if on(B,L1),L1 != L2
%%    -on(B2,B) if on(B1,B), B1 != B2
%%    above(B,L) if on(B,L)
%%    above(B,L) if on(B,B1), on(B1,L)
%% Executability conditions:
%%    impossible put(B,L) if on(B1,B)
%%    impossible put(B1,B) if on(B2,B)

%% Signature

%% Statics
block(b0). block(b1). block(b2).
block(b3). block(b4).

loc(t). loc(b0). loc(b1).
loc(b2). loc(b3). loc(b4).

fluent(inertial,on(B1,B2)) :- holds(on(B1,B2),I).
fluent(defined,above(B1,B2)) :- holds(above(B1,B2),I).

%% Setting step numbers
#const n=1.
step(0..n).

%% Encoding II(SD)

%% Causal law:
%%    put(B,L) causes on(B,L)
holds(on(B,L),I+1) :- occurs(put(B,L),I), I<n.

%% State constraints

%% -on(B,L2) if on(B,L),L1 != L2 - one block cannot be on two different locations
-holds(on(B,L2),I) :- holds(on(B,L1),I), L1 != L2, loc(L2).

%% on(B2,B) if on(B1,B), B1 != B2 - one block cannot be occupied by two different blocks
-holds(on(B2,B),I) :- holds(on(B1,B),I), B1 != B2, block(B), block(B2).

%% above(B,L) if on(B,L)
holds(above(B,L),I) :- holds(on(B,L),I).

%% above(B,L) if on(B,B1), on(B1,L)
holds(above(B,L),I) :- holds(on(B,B1),I), holds(on(B1,L),I).

%% Executability conditions:
%% impossible put(B,L) if on(B1,B)
-occurs(put(B,L),I) :- holds(on(B1,B),I), loc(L).



%% impossible put(B1,B) if on(B2,B)
-occurs(put(B1,B),I) :- holds(on(B2,B),I), B1 != B2, block(B), loc(B1).

%% CWA for defined fluent
-holds(F,I) :- fluent(defined,F), not holds(F,I), step(I).

%% inertial rule for inertial fluent
holds(F,I+1) :- fluent(inertial,F), holds(F,I), not -holds(F,I+1), I<n.
-holds(F,I+1) :- fluent(inertial,F), -holds(F,I), not holds(F,I+1), I<n.

%% CWA for action
-occurs(put(B1,B2),I) :- not occurs(put(B1,B2),I), step(I), block(B1), loc(B2).

%% Encoding SD initial state
holds(on(b0,t),0).
holds(on(b3,b0),0).
holds(on(b2,b3),0).
holds(on(b1,t),0).
holds(on(b4,b1),0).


%% Encoding action
occurs(put(b2,b4),0).


%#show holds/2.
%#show fluent/2.
%#show -holds/2.