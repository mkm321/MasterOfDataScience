%% define blocks
block(r1). block(y1). block(b1). block(g1). block(b2). block(g2).
block(r2). block(y2). block(b3). block(r3). block(g3).

%% define locations
loc(r1). loc(y1). loc(b1). loc(g1). loc(b2). loc(g2).
loc(r2). loc(y2). loc(b3). loc(r3). loc(g3). loc(t).

%% Initial Configuration
holds(on(g3,t), 0). holds(on(r3,g3), 0). holds(on(b3,r3), 0).
holds(on(y2,b3), 0). holds(on(r2,y2), 0). holds(on(g2,r2), 0).
holds(on(b2,g2), 0). holds(on(g1,b2), 0). holds(on(b1,g1), 0).
holds(on(y1,b1), 0). holds(on(r1,y1), 0).

%%% block not on location L at step 0, CWA
-holds(on(B,L), 0) :- not holds(on(B,L), 0), block(B), loc(L).

%% Putting block B on location L at step I causes B to be on L at step
%% I+1
holds(on(B,L), I+1) :- occurs(put(B,L), I).

%% A block cannot be in two location at once
-holds(on(B,L2), I) :- holds(on(B,L1), I), L1 != L2.

%% Only one block can be directly on top of one another.
-holds(on(B2,B), I) :- block(B2), holds(on(B1,B), I), B1 != B2.

%% Inertia rule 1: Anything that holds at step I, will also hold at step
%% I+1 as long as no evidence shows its opposite.
holds(F,I+1) :- holds(F,I), not -holds(F,I+1), I<n.

%% Inertia rule 2: Anything that does not hold at step I, will also not
%% hold at step I+1, as long as no evidence shows its opposite.
-holds(F,I+1) :- -holds(F,I), not holds(F,I+1), I<n.

%% impossible actions
%% it is impossible to move an occupied block:
-occurs(put(B,t),I) :- holds(on(B1,B),I).
-occurs(put(B,B2),I) :- holds(on(B1,B),I),
                          holds(on(B2,B3),I),
                          B1 != B2, B != B3.

%% It is impossible to move a block onto an occupied block:
-occurs(put(B1,B),I) :- holds(on(B1,L),I),
                          holds(on(B2,B),I),
                          B1 != B2, B != t.



%% action to be placed at step 0.