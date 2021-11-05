%% Signature
%% Statics

%% Cube's colours are defined as the name r1,y1 etc.
%% define cubes
cube(r1). cube(y1). cube(b1). cube(g1). cube(b2). cube(g2).
cube(r2). cube(y2). cube(b3). cube(r3). cube(g3).

%% define locations
loc(r1). loc(y1). loc(b1). loc(g1). loc(b2). loc(g2).
loc(r2). loc(y2). loc(b3). loc(r3). loc(g3). loc(t).


%% Intertial and defined fluents

fluent(inertial,on(C1,C2)) :- holds(on(C1,C2),I).
fluent(defined,above(C1,C2)) :- holds(above(C1,C2),I).


%% Setting step numbers

#const n=11.
step(0..n).


%%% cube not on location L at step 0, CWA
-holds(on(C1,L1), 0) :- not holds(on(C1,L1), 0), cube(C1), loc(L1).

%% Encoding SD

%% Causal Law

%% Putting cube B on location L at step I causes B to be on L at step I+1
holds(on(C1,L1), I+1) :- occurs(put(C1,L1), I).


%% State Constraints

%% if removal of a cube occur at step I, then it would not hold any
%% relation at step I+1
-holds(on(C1,L1), I+1) :- occurs(remove(C1), I), cube(C1), loc(L1).
removed(C1) :- occurs(remove(C1), I), cube(C1).

%% A cube cannot be in two location at once
-holds(on(C1,L2), I) :- holds(on(C1,L1), I), L1 != L2, loc(L2).

%% Only one cube can be directly on top of one another.
-holds(on(C2,C),I) :- holds(on(C1,C),I), C1 != C2, cube(C), cube(C2).

%% above(C1,L1) if on(C1,L1)
holds(above(C1,L1),I) :- holds(on(C1,L1),I).

%% above(C1,L1) if on(C1,C2), on(C2,L1)
holds(above(C1,L1),I) :- holds(on(C1,C2),I), holds(on(C2,L1),I).

%% Executability conditions:

%% It is impossible to move an occupied cube:
-occurs(put(C,T),I) :- holds(on(C1,C),I), loc(T).
-occurs(put(C,C2),I) :- holds(on(C1,C),I),
                          holds(on(C2,C3),I),
                          C1 != C2, C != C3.
%% It is impossible to move a cube onto
%% an occupied cube:
-occurs(put(C1,C),I) :- holds(on(C1,L),I),
                          holds(on(C2,C),I),
                          C1 != C2, C != t.


%% CWA for defined fluent
-holds(F,I) :- fluent(defined,F), not holds(F,I), step(I).
%% CWA for removed 
-removed(B) :- not removed(B), cube(B).

%% Inertia rules

%% Inertia rule 1: Anything that holds at step I, will also hold at step I+1 as long as no evidence shows its opposite.
holds(F,I+1) :- fluent(inertial,F), holds(F,I), not -holds(F,I+1), I<n.

%% Inertia rule 2: Anything that does not hold at step I, will also not hold at step I+1, as long as no evidence shows its opposite.
-holds(F,I+1) :- fluent(inertial,F), -holds(F,I), not holds(F,I+1), I<n.

%% Initial Configuration
holds(on(g3,t), 0). holds(on(r3,g3), 0). holds(on(b3,r3), 0).
holds(on(y2,b3), 0). holds(on(r2,y2), 0). holds(on(g2,r2), 0).
holds(on(b2,g2), 0). holds(on(g1,b2), 0). holds(on(b1,g1), 0).
holds(on(y1,b1), 0). holds(on(r1,y1), 0).


%% Encoding action
occurs(put(r1,t),0).
occurs(remove(y1),1).
occurs(put(b1,t),2).
occurs(put(g1,t),3).

occurs(put(b2,b1),4).
occurs(put(g2,g1),5).
occurs(put(r2,r1),6).
occurs(remove(y2),7).

occurs(put(b3,b2),8).
occurs(put(r3,r2),9).
occurs(put(g3,g2),10).

%#show holds/2.
#show -removed/1.
%#show removed/1.