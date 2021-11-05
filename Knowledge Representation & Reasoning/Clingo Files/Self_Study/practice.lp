%% Signature
%% Statics

%% define cubes
cube(red,r1). cube(yellow,y1). cube(blue,b1). cube(green,g1). cube(blue,b2). 
cube(green,g2). cube(red,r2). cube(yellow,y2). cube(blue,b3). cube(red,r3). 
cube(green,g3).

%% define locations
loc(r1). loc(y1). loc(b1). loc(g1). loc(b2). loc(g2).
loc(r2). loc(y2). loc(b3). loc(r3). loc(g3). loc(t).


%% Intertial and defined fluents

fluent(inertial,on(C,C1,C2)) :- holds(on(C,C1,C2),I).
fluent(defined,above(C,C1,C2)) :- holds(above(C,C1,C2),I).


%% Setting step numbers

#const n=11.
step(0..n).


%%% cube not on location L at step 0, CWA
-holds(on(C,C1,L1), 0) :- not holds(on(C,C1,L1), 0), cube(C,C1), loc(L1).

%% Encoding SD

%% Causal Law

%% Putting cube B on location L at step I causes B to be on L at step I+1
holds(on(C,C1,L1), I+1) :- occurs(put(C,C1,L1), I).

removed(C1) :- occurs(remove(C,C1), I), cube(C,C1).

%% State Constraints

%% if removal of a cube occur at step I, then it would not hold any
%% relation at step I+1
-holds(on(C,C1,L1), I+1) :- occurs(remove(C,C1), I), cube(C,C1), loc(L1).


%% A cube cannot be in two location at once
-holds(on(C,C1,L2), I) :- holds(on(C,C1,L1), I), L1 != L2, loc(L2).

%% Only one cube can be directly on top of one another.
-holds(on(CO,C2,C),I) :- holds(on(CO,C1,C),I), C1 != C2, cube(CO,C), cube(CO,C2).

%% above(C1,L1) if on(C1,L1)
holds(above(C,C1,L1),I) :- holds(on(C,C1,L1),I).

%% above(C1,L1) if on(C1,C2), on(C2,L1)
holds(above(C,C1,L1),I) :- holds(on(C,C1,C2),I), holds(on(CO,C2,L1),I).

%% colours can not be same

-holds()

%% Executability conditions:

%% It is impossible to move an occupied cube:
-occurs(put(CO,C,T),I) :- holds(on(CO,C1,C),I), loc(T).

%% It is impossible to move a cube onto
%% an occupied cube:
-occurs(put(CO,C1,C),I) :- holds(on(CO,C1,L),I),
                          holds(on(CL,C2,C),I),
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
holds(on(green,g3,t), 0). holds(on(red,r3,g3), 0). holds(on(blue,b3,r3), 0).
holds(on(yellow,y2,b3), 0). holds(on(red,r2,y2), 0). holds(on(green,g2,r2), 0).
holds(on(blue,b2,g2), 0). holds(on(green,g1,b2), 0). holds(on(blue,b1,g1), 0).
holds(on(yellow,y1,b1), 0). holds(on(red,r1,y1), 0).


%% Encoding action
occurs(put(red,r1,t),0).
occurs(remove(yellow,y1),1).
occurs(put(blue,b1,t),2).
occurs(put(green,g1,t),3).

occurs(put(blue,b2,b1),4).
occurs(put(green,g2,g1),5).
occurs(put(red,r2,r1),6).
occurs(remove(yellow,y2),7).

occurs(put(blue,b3,b2),8).
occurs(put(red,r3,r2),9).
occurs(put(green,g3,g2),10).

#show holds/2.
%#show -removed/1.
%#show removed/1.