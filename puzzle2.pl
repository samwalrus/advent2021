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

plan2(p(0,0,0)) --> [].
plan2(p(F1,D1,Aim0)) --> [i(forward,Amount)],plan2(p(F0,D0,Aim0)),{ F1 #= F0 + Amount,D1#=D0+Aim0*Amount }.
plan2(p(F0,D0,Aim)) --> [i(down,Amount)],plan2(p(F0,D0,Aim0)),{ Aim #= Aim0 + Amount }.
plan2(p(F0,D0,Aim)) --> [i(up,Amount)],plan2(p(F0,D0,Aim0)),{ Aim #= Aim0 - Amount }.


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
%%% ?- test(X), reverse(X,R),phrase(plan2(P),R).

%%% csv_read_file('input2.csv', Rows, [functor(i),separator(0' )]),phrase(plan(p(F,D)),Rows),Total #=F*D.
%%% csv_read_file('input2.csv', Rows, [functor(i),separator(0' )]), reverse(Rows,R),phrase(plan2(p(F,D,A)),R),Total #= F*D.