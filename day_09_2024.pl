
%%% 
%%%
%%% So this is presumably the point at which a direct
%%% implementation becomes unwieldy. Let's start with
%%% a naïve implementation, though.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library imports

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AoC boilerplate

aoc_09_test_part1(Out):-
    solve_aoc_09_part1("data/data_09_2024_test.txt", Out).

aoc_09_part1(Out):-
    solve_aoc_09_part1("data/data_09_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

solve_aoc_09_part1(FileName, Out):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "", "\n\s\t", [InputString]),
    input_to_list(InputString, InputList),
    compact_list(InputList, CompactList),

    length(CompactList, L),
    L1 is L-1,
    numlist(0, L1, Idxs),
    maplist([X, Y, Z]>>(Z is (X*Y)), CompactList, Idxs, Products),
    sumlist(Products, Out).
    

%%% There seems to be a reasonably neat solution just
%%% by reversing the input. Bit of a hack by chopping
%%% off the number of actual numbers in the input,
%%% rather than find an elegant terminating condition :-).

compact_list(InputList, CompactList):-
    reverse(InputList, RevList),
    compact_list(InputList, RevList, AllCompactList),
    include(integer, InputList, FileBlocks),
    same_length(CompactList, FileBlocks),
    append(CompactList, _, AllCompactList).

compact_list([], _, []).
compact_list([Num|Rest], RevList, [Num|Out]):-
    integer(Num),
    compact_list(Rest, RevList, Out).
compact_list(['.'|Rest], ['.'|RevRest], Out):-
    compact_list(['.'|Rest], RevRest, Out).
compact_list(['.'|Rest], [Num|RevRest], [Num|Out]):-
    integer(Num),
    compact_list(Rest, RevRest, Out).
    

input_to_list(String, Out):-
    string_chars(String, Chars),
    maplist(atom_number, Chars, Lengths),
    maplist([X, Y]>>length(Y, X), Lengths, L1),
    instantiate_lists(L1),
    flatten(L1, Out).

instantiate_lists(Lists):-
    instantiate_lists(Lists, 0).

instantiate_lists([LastList], Num):-
    maplist(=(Num), LastList).
instantiate_lists([Files, Gaps|Lists], Num):-
    maplist(=(Num), Files),
    maplist(=('.'), Gaps),
    succ(Num, Num1),
    instantiate_lists(Lists, Num1).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:

aoc_09_test_part2(Out):-
    solve_aoc_09_part2("data/data_09_2024_test.txt", Out).

aoc_09_part2(Out):-
    solve_aoc_09_part2("data/data_09_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

t(X):-
    aoc_09_test_part2(X).

solve_aoc_09_part2(FileName, Out):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "", "\n\s\t", [InputString]),
    input_to_list(InputString, InputList),
    compact_list_2(InputList, CompactList),

    length(CompactList, L),
    L1 is L-1,
    numlist(0, L1, Idxs),
    maplist([X, Y, Z]>>(Z is (X*Y)), CompactList, Idxs, Products),
    sumlist(Products, Out).

    
    
compact_list_2(InputList, Out):-
    clumped(InputList, ClumpedList),
    maplist([X, Y]>>arg(1, X, Y), ClumpedList, ClumpIds),
    include(integer, ClumpIds, RevFileIds),
    reverse(RevFileIds, FileIds),
    compact_list_2(ClumpedList, FileIds, CompactedClumpedList),
    declump_with_blank(CompactedClumpedList, Out).

compact_list_2(FinalState, [], FinalState).
compact_list_2(ClumpedList, [FileIdx|Rest], Out):-
    move_file(ClumpedList, FileIdx, CL),!,
    compact_list_2(CL, Rest, Out).
compact_list_2(ClumpedList, [FileIdx|Rest], Out):-
    \+ move_file(ClumpedList, FileIdx, _),
    compact_list_2(ClumpedList, Rest, Out).


move_file(ClumpedList, FileIdx, Out):-
    append(Before, [FileIdx-FileSize|Rest], ClumpedList),
    append(B1, ['.'-GapSize|B2], Before),
    GapSize>=FileSize,
    Pad is GapSize-FileSize,
    append(B1, [FileIdx-FileSize, '.'-Pad|B2], Before2),
    append(Before2, ['.'-FileSize|Rest], ReplacedTerm),
    smooth(ReplacedTerm, Out),
    !. % Don't want to backtrack...

% Smooth a clumped list to combine adjacent clumps,
% and remove clumps of size zero
smooth([X-R], [X-R]).
smooth([X-R, Y-S|Rest], [X-R|Out]):-
    X \= Y,
    R \= 0,
    smooth([Y-S|Rest], Out).
smooth([_-0|Rest], Out):-
    smooth(Rest, Out).
smooth([X-R, X-S|Rest], Out):-
    T is R+S,
    smooth([X-T|Rest], Out).

declump_with_blank([], []).
declump_with_blank([X-R|Rest], Out):-
    integer(X),
    length(Xs, R),
    maplist(=(X), Xs),
    declump_with_blank(Rest, RestDeclumped),
    append(Xs, RestDeclumped, Out).
declump_with_blank(['.'-R|Rest], Out):-
    length(Xs, R),
    maplist(=(0), Xs),
    declump_with_blank(Rest, RestDeclumped),
    append(Xs, RestDeclumped, Out).
