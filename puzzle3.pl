:-use_module(library(csv)).
:-use_module(library(yall)).
:-use_module(library(clpfd)).
:-use_module(library(reif)).

test(X):- X=['00100','11110','10110','10111','10101','01111','00111','11100','10000','11001','00010','01010'].

binary_number(Bs0, N) :-
        reverse(Bs0, Bs),
        foldl(binary_number_, Bs, 0-0, _-N).

binary_number_(B, I0-N0, I-N) :-
        B in 0..1,
        N #= N0 + B*2^I0,
        I #= I0 + 1.

i_atom(i(Thing),Thing).

numbers_codes(Nums,Codes):-
    maplist(char_code,Ns,Codes),
    maplist(atom_number,Ns,Nums).
%?- csv_read_file('input3.csv', Rows, [functor(i),separator(0' )]).


thing2(I,Ns,Firsts):-maplist(nth1(I),Ns,Firsts).

list_mcommon_lcommon(List,MaxValue,MinValue):-
    aggregate_all(count, member(1,List), Result),
    aggregate_all(count, member(0,List), Result2),
    pairs_keys_values(Pairs,[Result,Result2],[1,0]),
    list_to_assoc(Pairs,Assoc),
    max_assoc(Assoc, MaxKey, MaxValue),
    min_assoc(Assoc, MinKey, MinValue).

ns_index_max_min(Ns,I,MaxValue,MinValue):-
    maplist(nth1(I),Ns,Firsts),
    list_mcommon_lcommon(Firsts,MaxValue,MinValue).

:- meta_predicate tcount(2,?,?).
tcount(P_2,Xs,N) :-
   N #>= 0,
   list_pred_tcount_(Xs,P_2,0,N).

:- meta_predicate list_pred_tcount_(?,2,?,?).
list_pred_tcount_([]    , _ ,N ,N).
list_pred_tcount_([X|Xs],P_2,N0,N) :-
   if_(call(P_2,X), (N1 is N0+1, N1 #=< N), N1 = N0),
   list_pred_tcount_(Xs,P_2,N1,N).


%?- test(X),maplist(atom_codes,X,Codes),maplist(numbers_codes,Ns,Codes), Codes=[H|T],length(H,Len),numlist(1,Len,Numlist),maplist(ns_index_max_min(Ns),Numlist,Max,Min),binary_number(Max,Gamma),binary_number(Min,Ep),PC #= Gamma*Ep.

%?- csv_read_file('input3.csv', Rows, [functor(i),separator(0' ),convert(false)]),maplist(i_atom,Rows,Atoms), maplist(atom_codes,Atoms,Codes),maplist(numbers_codes,Ns,Codes).
