qcomp(S,C) :- findall(X,setup(S,X,_,_),C).

qmark(S,N,as1,M) :-     c325(S,N,M,_,_,_,_,_).
qmark(S,N,as2,M) :-     c325(S,N,_,M,_,_,_,_).
qmark(S,N,as3,M) :-     c325(S,N,_,_,M,_,_,_).
qmark(S,N,as4,M) :-     c325(S,N,_,_,_,M,_,_).
qmark(S,N,midterm,M) :- c325(S,N,_,_,_,_,M,_).
qmark(S,N,final,M) :-   c325(S,N,_,_,_,_,_,M).

q1h(+S,-C,+N,-T).
q1h(S,[],N,0).
q1h(S,[H|C],N,T) :- setup(S,H,TM,W), qmark(S,N,H,M), q1h(S,C,N,T1), T is T1+W*(M/TM).

query1(S,N,T):-qcomp(S,C), q1h(S,C,N,T).

mfcomp(S,M,F) :- setup(S,midterm,TM,_), setup(S,final,TF,_), (M/TM)<(F/TF).

q2h(S,N):-qmark(S,N,midterm,MT), qmark(S,N,final,FN), mfcomp(S,MT,FN).

query2(S,L) :- findall(X,q2h(S,X),L).

qexist(S,N) :- c325(S,N,_,_,_,_,_,_).

q3h(S,N,as1,NM) :-      retract(c325(S,N,A1,A2,A3,A4,MT,FN)), assert(c325(S,N,NM,A2,A3,A4,MT,FN)).
q3h(S,N,as2,NM) :-      retract(c325(S,N,A1,A2,A3,A4,MT,FN)), assert(c325(S,N,A1,NM,A3,A4,MT,FN)).
q3h(S,N,as3,NM) :-      retract(c325(S,N,A1,A2,A3,A4,MT,FN)), assert(c325(S,N,A1,A2,NM,A4,MT,FN)).
q3h(S,N,as4,NM) :-      retract(c325(S,N,A1,A2,A3,A4,MT,FN)), assert(c325(S,N,A1,A2,A3,NM,MT,FN)).
q3h(S,N,midterm,NM) :-  retract(c325(S,N,A1,A2,A3,A4,MT,FN)), assert(c325(S,N,A1,A2,A3,A4,NM,FN)).
q3h(S,N,final,NM) :-    retract(c325(S,N,A1,A2,A3,A4,MT,FN)), assert(c325(S,N,A1,A2,A3,A4,MT,NM)).

query3(S,N,C,NM) :- \+ qexist(S,N), print('record not found'), !.
query3(S,N,C,NM) :- q3h(S,N,C,NM).