#const steps=15.

car(car1).
car(car2).
car(car3).
car(car4).

location(a).
location(b).
location(c).
location(d).
location(e).
location(f).
location(g).
location(h).

at(car1,a,0).
at(car2,b,0).
at(car3,c,0).
at(car4,d,0).

at(ferry,h,0).

goal(T):-
at(car1,e,T),
at(car2,f,T),
at(car3,g,T),
at(car4,h,T).