sub(narwhal).
is_a(narwhal,sub).
is_subclass(sub,water).
is_subclass(ship,water).
can_swim(narwhal).
is_subclass(water,vehicle).
is_subclass(car,land).
is_subclass(truck,land).
is_subclass(airplane,air).
is_subclass(air,vehicle).
is_subclass(land,vehicle).
car(abby).
is_a(abby,car).
atom(narwhal).
atom(abby).

float(X) :- can_swim(X), member(X,water), atom(X).
-float(X) :- not can_swim(X),not member(X,water), atom(X).
vehicle(X) :- sub(X).
-car(X) :- not car(X), atom(X).
-sub(X) :- not sub(X), atom(X).
subclass(C1,C2) :- is_subclass(C1,C2).
subclass(C1,C2) :- is_subclass(C1,C3), subclass(C3,C2).
-subclass(C1,C2) :- not subclass(C1,C2), atom(C1,C2).
member(X,C) :- is_a(X,C).
member(X,C) :- is_a(X,C0), subclass(C0,C).
siblings(C1,C2) :- is_subclass(C1,C), is_subclass(C2,C), C1 != C2.
-member(X,C2) :- member(X,C1), siblings(C1,C2), C1 != C2.


#show float/1.
#show -float/1.