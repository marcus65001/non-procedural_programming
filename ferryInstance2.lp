#const steps=19.

car(car1).
car(car2).
car(car3).
car(car4).
car(car5).

location(a).
location(b).
location(c).
location(d).
location(e).
location(f).
location(g).
location(h).
location(i).
location(j).

at(car1,a,0).
at(car2,b,0).
at(car3,c,0).
at(car4,d,0).
at(car5,e,0).

at(ferry,j,0).

goal(T):-
at(car1,f,T),
at(car2,g,T),
at(car3,h,T),
at(car4,i,T),
at(car5,j,T).