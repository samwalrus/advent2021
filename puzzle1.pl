:-use_module(library(csv)).
:-use_module(library(yall)).
:-use_module(library(clpfd)).


comparisons([_],[],0).
comparisons([H,H2|Rest],[C|Comparrisons],Count):-
    zcompare(C,H,H2),
    C = <,
    Count #=Count0+1,
    comparisons([H2|Rest],Comparrisons,Count0).
comparisons([H,H2|Rest],[C|Comparrisons],Count):-
    zcompare(C,H,H2),
    comparisons([H2|Rest],Comparrisons,Count).

row_n(row(N),N).

list_windows([_,_],[]).
list_windows([A,B,C|Rest],[Sum|Window]):-
    Sum #= A+B+C,
    list_windows([B,C|Rest],Window).
    
    
example(
    [
        199,200,208,210,200,207,240,269,260,263 
    ]
).

example2([607,618,618,617,647,716,769,792]).
  
%?- csv_read_file('input1.csv', Rows, []),maplist(row_n,Rows,Ns),comparisons(Ns,Cs,Count), length(Rows,L).
%?- csv_read_file('input1.csv', Rows, []),maplist(row_n,Rows,Ns),list_windows(Ns,Ws),comparisons(Ws,Cs,Count), length(Rows,L).
