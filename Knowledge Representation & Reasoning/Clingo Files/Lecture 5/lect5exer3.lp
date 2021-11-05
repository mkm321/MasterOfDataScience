is_subclass(submarine, water).
is_subclass(ship, water).
is_subclass(truck, land).
is_subclass(car, land).
is_subclass(airplane, air).
is_subclass(water, vehicle).
is_subclass(land, vehicle).
is_subclass(air, vehicle).
subclass(X,Y) :- is_subclass(X,Y).
subclass(X,Y) :- is_subclass(X,Z),
                 subclass(Z,Y).
atom(narwhal).
atom(abby).
atom(submarine).
atom(ship).
atom(truck).
atom(car).
atom(airplane).
atom(water).
atom(land).
atom(air).
atom(vehicle).
-subclass(X,Y) :- not subclass(X,Y),
                  atom(X), atom(Y).
is_a(narwhal, submarine).
is_a(abby, car).
member(X,Y) :- is_a(X,Y).
member(X,Y) :- is_a(X,Z),
               subclass(Z,Y).
siblings(C1,C2) :- is_subclass(C1,C),
                   is_subclass(C2,C), 
                   C1 != C2.
-member(X,C2) :- member(X,C1),
                 siblings(C1,C2),
                 C1 != C2.

move_on_water(X) :- member(X,water).
move_on_water(X) :- subclass(X,water).
-move_on_water(X) :- not move_on_water(X),
                     atom(X).
#show -move_on_water/1.
#show move_on_water/1.