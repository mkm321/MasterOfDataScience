%% Scenario: Suppose there is a mythical country, called u, whose inhabitants don’t care about 
%% their children. Suppose our knowledge base contains information about the national 
%% origin of most but not all recorded people.
%% Pit and Kathy are Jim’s parents. Kathy was born in Moldova, but we don’t know where 
%% Pit is from. He could have been born in country u.
%% We also assume that both parents have been seen at school, 
%% so the absence thing doesn’t come into play.

parent(pit,jim).
parent(kathy,jim).
atom(pit,jim).
atom(kathy,jim).
born_in(kathy,maldova).
country(maldova).
country(u).

-born_in(P,C1) :- born_in(P,C2), C1!= C2, country(C1), country(C2).

%% The original default
cares(X,Y) :- parent(X,Y),
                not ab(d_cares(X,Y)),
                not -cares(X,Y).

%% strong exception
-cares(X,Y) :- parent(X,Y), born_in(X,u).
%% weak exception
ab(d_cares(X,Y)) :- not -born_in(X,u), atom(X,Y).