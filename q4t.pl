/* 
The goal of Sudoku is to fill in a 9 by 9 grid with digits 
so that each column, row, and 3 by 3 section contain the 
numbers between 1 to 9. At the beginning of the game, 
the 9 by 9 grid will have some of the squares filled in. 
Your job is to fill in the missing digits and complete the grid. 

*/

:- use_module(library(clpfd)).

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

sudoku(Rows) :-
    grid(9, Rows),
        % Rows now is a 9x9 grid of variables
    append(Rows, Vs),
        % Vs is a list of all 9*9 variables in Rows
    Vs ins 1..9,
    xall-distinct(Rows),
        % Variables of each row get distinct values
    xtranspose(Rows, Columns),
        % get the columns of 9x9 grid
    xall-distinct(Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        % need references to rows
    blocks(As, Bs, Cs),
        % deal with three rows at a time
    blocks(Ds, Es, Fs),
    blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
    all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
    blocks(Ns1, Ns2, Ns3).

problem(P) :-
    P = [[1,_,_,8,_,4,_,_,_],
	 [_,2,_,_,_,_,4,5,6],
	 [_,_,3,2,_,5,_,_,_],
	 [_,_,_,4,_,_,8,_,5],
	 [7,8,9,_,5,_,_,_,_],
	 [_,_,_,_,_,6,2,_,3],
	 [8,_,1,_,_,_,7,_,_],
	 [_,_,_,1,2,3,_,8,_],
	 [2,_,5,_,_,_,_,_,9]].

t(Rows) :-
    problem(Rows),
    sudoku(Rows),
    maplist(labeling([ff]), Rows),
    maplist(writeln, Rows).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Example:
   ?- t(Rows).
   [1,5,6,8,9,4,3,2,7]
   [9,2,8,7,3,1,4,5,6]
   [4,7,3,2,6,5,9,1,8]
   [3,6,2,4,1,7,8,9,5]
   [7,8,9,3,5,2,6,4,1]
   [5,1,4,9,8,6,2,7,3]
   [8,3,1,5,4,9,7,6,2]
   [6,9,7,1,2,3,5,8,4]
   [2,4,5,6,7,8,1,3,9]
   Rows = [[1, 5, 6, 8, 9, 4, 3, 2|...], [9, 2, 8, 7, 3, 1, 4|...], [4, 7, 3, 2, 6, 5|...], [3, 6, 2, 4, 1|...], [7, 8, 9, 3|...], [5, 1, 4|...], [8, 3|...], [6|...], [...|...]].

*/