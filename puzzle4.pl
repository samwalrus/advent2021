:- use_module(library(pure_input)).
:- use_module(library(http/dcg_basics)).


test(X):-
    X = puzzle([1,2,3],[]).

file --> input,boards.

input --> true.%a single line

boards --> board, boards.

board --> true. % A grid.


numbers_winningboard_score(N,W,Score):-
    
    true.


thing :- member(Row,Rows).
thing :- transpose(Board,TBoard),member(Row,Rows).

board_numbers_score(B,N,S):-
    filter(B,N,Notmarked),
    sumlist(Notmarked,Sum),
    last(Numbers,Last)
    Score #= Sum * Last.
    


