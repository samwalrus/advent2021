% Constraint Logic Programming
:- use_module(library(dif)).	% Sound inequality
:- use_module(library(clpfd)).	% Finite domain constraints
:- use_module(library(clpb)).	% Boolean constraints
:- use_module(library(chr)).	% Constraint Handling Rules
:- use_module(library(when)).	% Coroutining
%:- use_module(library(clpq)).  % Constraints over rational numbers
:- use_module(library(http/dcg_basics)).

% Your program goes here


/** <examples> Your example queries go here, e.g.
?- X #> 1.
*/

v_v1(0,6).
v_v1(X,X1):- dif(X,0),X1 #= X-1.

s_s1(Inputs,Outputs):-
    findall(8,member(0,Inputs),New),
    maplist(v_v1,Inputs,NewInputs),
    append(NewInputs,New,Outputs).

state(S0, S), [S] --> [S0].

inputlist --> state(S0,S),{s_s1(S0,S)}.

in(X,0) --> remainder(X).
in(X,Count) --> {Count0 #= Count -1},inputlist,in(X,Count0).

input_days_fishes(I,D,F):-
    phrase(in([X],D),[I],_), length(X,F).

%?- phrase(in([X],80),[[3,4,3,1,2]],Y), length(X,Fishes).
%?- input_days_fishes([3,4,3,1,2],80,F).
%@ F = 5934 .

input(X):-
    X =[1,1,1,2,1,5,1,1,2,1,4,1,4,1,1,1,1,1,1,4,1,1,1,1,4,1,1,5,1,3,1,2,1,1,1,2,1,1,1,4,1,1,3,1,5,1,1,1,1,3,5,5,2,1,1,1,2,1,1,1,1,1,1,1,1,5,4,1,1,1,1,1,3,1,1,2,4,4,1,1,1,1,1,1,3,1,1,1,1,5,1,3,1,5,1,2,1,1,5,1,1,1,5,3,3,1,4,1,3,1,3,1,1,1,1,3,1,4,1,1,1,1,1,2,1,1,1,4,2,1,1,5,1,1,1,2,1,1,1,1,1,1,1,1,2,1,1,1,1,1,5,1,1,1,1,3,1,1,1,1,1,3,4,1,2,1,3,2,1,1,2,1,1,1,1,4,1,1,1,1,4,1,1,1,1,1,2,1,1,4,1,1,1,5,3,2,2,1,1,3,1,5,1,5,1,1,1,1,1,5,1,4,1,2,1,1,1,1,2,1,3,1,1,1,1,1,1,2,1,1,1,3,1,4,3,1,4,1,3,2,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,2,1,5,1,1,1,1,2,1,1,1,3,5,1,1,1,1,5,1,1,2,1,2,4,2,2,1,1,1,5,2,1,1,5,1,1,1,1,5,1,1,1,2,1].


%?- input(X),input_days_fishes(X,80,F).
%@ X = [1, 1, 1, 2, 1, 5, 1, 1, 2|...],
%@ F = 391671 
%@ Unknown action:  (h for help)
%@ Action? 
%@ Unknown action:  (h for help)
%@ Action? .


%?- input(X),input_days_fishes(X,256,F).
%@ ERROR: Stack limit (1.0Gb) exceeded
%@ ERROR:   Stack sizes: local: 0.2Gb, global: 0.5Gb, trail: 13.7Mb
%@ ERROR:   Stack depth: 106, last-call: 6%, Choice points: 898,932
%@ ERROR:   In:
%@ ERROR:     [106] system:'$length'(_133532058, 116421)
%@ ERROR:     [104] user:s_s1([length:899,229], _133532080)
%@ ERROR:     [103] user:inputlist([length:1], _133532106)
%@ ERROR:     [102] user:in([length:1], 166, [length:1], _133532136)
%@ ERROR:     [101] user:in([length:1], 167, [length:1], _133532172)
%@ ERROR: 
%@ ERROR: Use the --stack_limit=size[KMG] command line option or
%@ ERROR: ?- set_prolog_flag(stack_limit, 2_147_483_648). to double the limit.
