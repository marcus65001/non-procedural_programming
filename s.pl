% Q1
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

% Q2
% [Written ans.]


% Q3
:- use_module(library(clpfd)).

gendigit2([W],0,[W]).
gendigit2([W|R],N,[X|O]) :- P is 10**N, X = W*P, NN is N-1, gendigit2(R,NN,O).
gendigit(W,O) :- length(W,L), LN is L-1, gendigit2(W,LN,O).

gensum2([],T,T).
gensum2([H|L],T,O) :- gensum2(L,T+H,O).
gensum(L,O) :- gensum2(L,0,O).

genterm(L,O) :- gendigit(L,O1), gensum(O1,O).

encrypt(W1,W2,W3) :- 
    length(W1,N),           % if you need to know the lengths of words
    length(W3,N1),   
    append(W1,W2,W),
    append(W,W3,L),
    list_to_set(L,Letters),     % remove duplicates, a predicate in the list library
    [LeadLetter1|_] = W1,   % identify the leading letter to be set to non-zero
    [LeadLetter2|_] = W2,
    [LeadLetter3|_] = W3,
    !,                      % never need to redo the above
    Letters ins 0..9,
    all_different(Letters),
    LeadLetter1 #\= 0,
    LeadLetter2 #\= 0,
    LeadLetter3 #\= 0,
    genterm(W1,T1),
    genterm(W2,T2),
    genterm(W3,T3),
    T1 + T2 #= T3,
    label(Letters).


% Q4
grid(N,1,[O]) :- length(O,N), !.
grid(N,T,[NL|O]) :- length(NL,N), NT is T-1, grid(N,NT,O).