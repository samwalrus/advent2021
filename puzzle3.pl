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


list_mcommon_lcommon(List,MaxValue,MinValue):-
    aggregate_all(count, member(1,List), Result),
    aggregate_all(count, member(0,List), Result2),
    dif(Result,Result2),
    pairs_keys_values(Pairs,[Result,Result2],[1,0]),
    list_to_assoc(Pairs,Assoc),
    max_assoc(Assoc, MaxKey, MaxValue),
    min_assoc(Assoc, MinKey, MinValue).

list_mcommon_lcommon(List,1,1):-
    aggregate_all(count, member(1,List), Result),
    aggregate_all(count, member(0,List), Result).

ns_index_max_min(Ns,I,MaxValue,MinValue):-
    maplist(nth1(I),Ns,Firsts),
    list_mcommon_lcommon(Firsts,MaxValue,MinValue).


%?- test(X),maplist(atom_codes,X,Codes),maplist(numbers_codes,Ns,Codes), Codes=[H|T],length(H,Len),numlist(1,Len,Numlist),maplist(ns_index_max_min(Ns),Numlist,Max,Min),binary_number(Max,Gamma),binary_number(Min,Ep),PC #= Gamma*Ep.

%?- csv_read_file('input3.csv', Rows, [functor(i),separator(0' ),convert(false)]),maplist(i_atom,Rows,Atoms), maplist(atom_codes,Atoms,Codes),maplist(numbers_codes,Ns,Codes).

%?csv_read_file('input3.csv', Rows, [functor(i),separator(0' ),convert(false)]),maplist(i_atom,Rows,Atoms), maplist(atom_codes,Atoms,Codes),maplist(numbers_codes,Ns,Codes),Codes=[H|T],length(H,Len),numlist(1,Len,Numlist),maplist(ns_index_max_min(Ns),Numlist,Max,Min),binary_number(Max,Gamma),binary_number(Min,Ep),PC #= Gamma*Ep.


%%%Part 2.

%?- csv_read_file('input3.csv', Rows, [functor(i),separator(0' ),convert(false)]),maplist(i_atom,Rows,Atoms), maplist(atom_codes,Atoms,Codes),maplist(numbers_codes,Ns,Codes).

binlist(Ns):-
    csv_read_file('input3.csv', Rows, [functor(i),separator(0' ),convert(false)]),
    maplist(i_atom,Rows,Atoms),
    maplist(atom_codes,Atoms,Codes),
    maplist(numbers_codes,Ns,Codes).

t_binlist(Ns):-
    test(X),maplist(atom_codes,X,Codes),maplist(numbers_codes,Ns,Codes).

%binlist_oxrateing([],Oxrating,_Index):- writeln(broken).
binlist_oxrateing([Oxrating],Oxrating,_Index).
binlist_oxrateing(BinList,Oxrating,Index):-
    ns_index_max_min(BinList,Index,MaxValue,MinValue),
    dif(MaxValue,MinValue),
    filter(BinList,NewBinList,Index,MaxValue),
    %length(NewBinList,Length),
    %writeln(Length),
    Index2 #=Index+1,
    binlist_oxrateing(NewBinList,Oxrating,Index2).

binlist_oxrateing(BinList,Oxrating,Index):-
    ns_index_max_min(BinList,Index,1,1),
    filter(BinList,NewBinList,Index,1),
    %length(NewBinList,Length),
    %writeln(Length),
    Index2 #=Index+1,
    binlist_oxrateing(NewBinList,Oxrating,Index2).

binlist_co2rateing([C02_rating],C02_rating,_Index).
binlist_co2rateing(BinList,C02_rating,Index):-
    ns_index_max_min(BinList,Index,MaxValue,MinValue),
    dif(MaxValue,MinValue),
    filter(BinList,NewBinList,Index,MinValue),
    Index2 #=Index+1,
    binlist_co2rateing(NewBinList,C02_rating,Index2).
binlist_co2rateing(BinList,C02_rating,Index):-
    ns_index_max_min(BinList,Index,1,1),
    filter(BinList,NewBinList,Index,0),
    Index2 #=Index+1,
    binlist_co2rateing(NewBinList,C02_rating,Index2).


nth_equal_t(Index,Elem,List,true):-
    nth1(Index,List,Elem).

nth_equal_t(Index,Elem1,List,false):-
    nth1(Index,List,Elem2), dif(Elem1,Elem2).



filter(BinList,NewBinList,Index,Elem):-
    tfilter(nth_equal_t(Index,Elem),BinList,NewBinList).


list_answer(Ns,A):-
    binlist_oxrateing(Ns,Ox,1),
    binlist_co2rateing(Ns,C,1),
    binary_number(Ox,N1),
    binary_number(C,N2),
    A #= N1*N2.