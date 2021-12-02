:-use_module(library(csv)).
:-use_module(library(yall)).
:-use_module(library(clpfd)).

%%% Maybe change to use implict state
%state(S), [S] --> [S].
%state(S0, S), [S] --> [S0].

plan(p(0,0)) --> [].
plan(p(F1,D0)) --> [i(forward,Famount)],plan(p(F0,D0)),{ F1 #= F0 + Famount }.
plan(p(F0,D1)) --> [i(down,Damount)],plan(p(F0,D0)),{ D1 #= D0 + Damount }.
plan(p(F0,D1)) --> [i(up,Damount)],plan(p(F0,D0)),{ D1 #= D0 - Damount }.

test(X):-
    X= [
    i(forward,5),
    i(down,5),
    i(forward,8),
    i(up,3),
    i(down,8),
    i(forward,2)
    ].

%%% ?- test(X),phrase(plan(P),X).
%%% csv_read_file('input2.csv', Rows, [functor(i),separator(0' )]),phrase(plan(p(F,D)),Rows),Total #=F*D.
