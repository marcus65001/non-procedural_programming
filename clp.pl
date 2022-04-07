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
% Examples of removal of domain values in Example 1:
% 1.
%   The domain for grid (5,3) is initially {9,4}.
%   But we see in the same row, grid (5,6) has domain: {4}.
%   So choosing 4 for grid (5,3) would leave grid (5,6) with no eligible number.
%   Therefore we reduce the domain of grid (5,3) to {9}.
%   
% 2.
%   The domain for grid (1,6) is initially {1,4,7}.
%   But we see in the same column, grid (5,6) has domain: {4}.
%   So choosing 4 for grid (1,6) would leave grid (5,6) with no eligible number.
%   Therefore we reduce the domain of grid (1,6) to {1,7}.
%
% 3.
%   The domain for grid (5,8) is initially {1,3,4,5,6}.
%   But we see in the same row, grid (5,6) has domain: {4}.
%   So choosing 4 for grid (5,8) would leave grid (5,6) with no eligible number.
%   Therefore we reduce the domain of grid (5,8) to {1,3,5,6}.
%
% 4.
%   The domain for grid (9,6) is initially {4,7}.
%   But we see in the same column, grid (5,6) has domain: {4}.
%   So choosing 4 for grid (9,6) would leave grid (5,6) with no eligible number.
%   Therefore we reduce the domain of grid (9,6) to {7}.
%
% 5.
%   The domain for grid (8,7) is initially {1,7}.
%   But we see in the same column, grid (5,7) has domain: {1}.
%   So choosing 1 for grid (8,7) would leave grid (5,7) with no eligible number.
%   Therefore we reduce the domain of grid (8,7) to {7}.
%
%
% Example in Example 2 of domain values that cannot be removed by AC-3:
% For grid (1,1), its domain is {9,4}.
%
% The domains of other grids in the same row:
% {8, 4, 5, 6}, {8, 9, 4, 6}, {3, 4, 6}, {8, 3, 4, 6}, {8, 4, 5}
% So no matter which value grid (1,1) chose, it wouldn't make any of the grid in the same row impossible.
%
% The domains of other grids in the same column:
% {2, 4}, {9, 4}, {1, 4}, {1, 2, 3, 4, 9}
% Again, no matter which value grid (1,1) chose, it wouldn't make any of the grid in the same column impossible.
%
% The domains of other grids in the same box:
% {8, 4, 5, 6}, {8, 9, 4, 6}, {2, 4}, {2, 4, 6, 7, 8}, {9, 4}, {4, 5, 6, 7, 8}
% No matter which value grid (1,1) chose, it wouldn't make any of the grid in the same box impossible.
%
% Therefore we could not reduce the domain by applying AC-3 for grid (1,1).

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
grid(N,O):-grid(N,N,O).

lind(+L,+E,+I,-O).
lind([E|L],E,I,I) :- !.
lind([_|L],E,I,O) :- I2 is I+1, lind(L,E,I2,O).

lind(L,E,O) :- lind(L,E,1,O).

gcol([],I,[]).
gcol([R|P],I,[H|O]) :- lind(R,H,I), gcol(P,I,O).

xtranspose(P,I,[]):- length(P,L), IX is L+1, I = IX.
xtranspose(P,I,[C|O]):-gcol(P,I,C), IX is I+1, xtranspose(P,IX,O).
xtranspose(P,O):-xtranspose(P,1,O).

differ(_,[]).
differ(A,[B|L]) :-
    A #\= B,
    differ(A,L).

xall-distinct([]).
xall-distinct([A]):- \+ is_list(A).
xall-distinct([A|L]) :-
    \+ is_list(A),
    differ(A,L),
    xall-distinct(L).
xall-distinct([A|L]) :-
    is_list(A),
    xall-distinct(A).

% Q5
rvcons(I,[],[]):- plist(_,N), I#=N+1,!.
rvcons(I,[R1|W1],[R2|W2]) :- 
    R1#\=R2,  % two reviewers distinct

    % workload
    occur(R1,W1,L1), 
    occur(R2,W2,L2),
    workLoadAtMost(K),
    L1#=<K,
    L2#=<K,

    % subject area and no self-review
    paper(I,C1N,C2N,SA),
    rvquery(SA,SETR),
    % initially used "in_set" operator in CLPFD (swipl 8.x), but lab machines are using a older version of swipl (7.6.4).
    % if direct access into the library is not allowed, please kindly replace the two lines with the following lines.
    % R1 in_set SETR,
    % R2 in_set SETR,
    clpfd:domain(R1,SETR),
    clpfd:domain(R2,SETR),
    rvassoc(C1N,C1),
    rvassoc(C2N,C2),
    R1 #\= C1,
    R1 #\= C2,
    R2 #\= C1,
    R2 #\= C2,

    IX#=I+1,
    rvcons(IX,W1,W2).

occur(I,Vars,N) :-
    generate_list(I,Vars,L),
    sum(L,#=,N).

generate_list(_,[],[]).
generate_list(I,[A|R],[T|S]) :-
    (I #= A #==> T#=1),
    (I #\= A #==> T#=0),
       generate_list(I,R,S).

rvlist(L,N):-findall(X,reviewer(X,_,_),L), length(L,N).



rvquery(Q,L):-
    findall(X,reviewer(X,Q,_),L1),
    findall(X,reviewer(X,_,Q),L2), 
    append(L1,L2,LT), 
    rvassocall(LT,LT2),
    list_to_set(LT2,LT3),
    % initially used "list_to_fdset" in CLPFD (swipl 8.x), but lab machines are using a older version of swipl (7.6.4).
    % if direct access into the library is not allowed, please kindly replace the last line with the following line.
    % list_to_fdset(LT3,L).
    clpfd:list_to_domain(LT3,L).

rvassocall([],[]).
rvassocall([LH|L],[IH|I]):-rvassoc(LH,IH), rvassocall(L,I).
rvassoc(xxx,0).
rvassoc(N,I):-rvlist(L,_), lind(L,N,I).

plist(L,N):- findall(X,paper(X,_,_,_),L), length(L,N).

assign(W1,W2):-
    plist(LP,NP),
    rvlist(LR,NR),
    length(WT1,NP),
    length(WT2,NP),
    append(WT1,WT2,WT),
    WT ins 1..NR,
    rvcons(1,WT1,WT2),
    label(WT),
    rvassocall(W1,WT1),
    rvassocall(W2,WT2).

