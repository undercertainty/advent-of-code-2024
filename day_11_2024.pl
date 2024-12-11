

%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library imports

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(readutil)).
:- use_module(library(yall)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AoC map boilerplate

aoc_11_test_part1(Out):-
    solve_aoc_11_part1("data/data_11_2024_test.txt", Out).

aoc_11_part1(Out):-
    solve_aoc_11_part1("data/data_11_2024.txt", Out).


get_input_from_file(FileName, ListOfInts):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "\s", "\n\s\t", IntsAsStrings),
    maplist(atom_number, IntsAsStrings, ListOfInts).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff
%
% Part 1 looks fine, but I suspect I know where part 2's going...

t(X):-
    solve_aoc_11_part1("data/data_11_2024_test.txt", X).

solve_aoc_11_part1(FileName, Out):-
    get_input_from_file(FileName, Stones),
    get_nth_state(25, Stones, StonesOut),
    length(StonesOut, Out).

% No attempt at efficiency in first attempt:
blink(0, [1]).
blink(Int, [N1, N2]):-
    Int>0,
    number_codes(Int, Codes),
    length(Codes, NumCodes),
    0 is NumCodes rem 2,
    L is NumCodes // 2,
    length(Codes1, L),
    length(Codes2, L),
    append(Codes1, Codes2, Codes),
    number_codes(N1, Codes1),
    number_codes(N2, Codes2).
blink(Int, [N]):-
    Int>0,
    number_codes(Int, Codes),
    length(Codes, NumCodes),
    1 is NumCodes rem 2,
    N is Int * 2024.


update_stones([], []).
update_stones([Next|Rest], Out):-
    blink(Next, UpdatedStone),
    update_stones(Rest, UpdatedStones),
    append(UpdatedStone, UpdatedStones, Out).

get_nth_state(0, State, State).
get_nth_state(N, InitialState, StateOut):-
    print(N),
    nl,
    N > 0,
    N1 is N-1,
    update_stones(InitialState, NextState),
    get_nth_state(N1, NextState, StateOut).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:
%
% Same but bigger. Thought so. Although usually it's
% 25 million rather than 75...

aoc_11_test_part2(Out):-
    solve_aoc_11_part2("data/data_11_2024_test.txt", Out).

aoc_11_part2(Out):-
    solve_aoc_11_part2("data/data_11_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Part 2's too slow to do by force, and a left hand tree
% expansion blows the stack.
%
% Dynamic programming then...

solve_aoc_11_part2(FileName, Out):-
    retractall(visited(_, _, _)),
    get_input_from_file(FileName, Stones),
    maplist([X]>>search(X, 75, 0), Stones),
    findall(Num, visited(_, 0, Num), Nums),
    sumlist(Nums, Out).


:- dynamic(visited/3).

t(N, TargetDepth):-
    retractall(visited(_, _, _)),
    search(N, 0, TargetDepth).

search(Num, TargetDepth, TargetDepth):-
    visited(Num, TargetDepth, _)
    ;
    assert(visited(Num, TargetDepth, 1)).

search(Num, _TargetDepth, Depth):-
    visited(Num, Depth, _).

search(0, TargetDepth, Depth):-
    \+ visited(0, Depth, _),
    Depth<TargetDepth,
    succ(Depth, D1),
    search(1, TargetDepth, D1),
    visited(1, D1, Num),
    assert(visited(0, Depth, Num)).

search(N, TargetDepth, Depth):-
    \+ visited(N, Depth, _),
    Depth<TargetDepth,
    N > 0,
    number_codes(N, Codes),
    length(Codes, NumCodes),
    0 is NumCodes rem 2,
    L is NumCodes // 2,
    length(Codes1, L),
    length(Codes2, L),
    append(Codes1, Codes2, Codes),
    number_codes(N1, Codes1),
    number_codes(N2, Codes2),
    succ(Depth, D1),
    search(N1, TargetDepth, D1),
    search(N2, TargetDepth, D1),
    visited(N1, D1, S1),
    visited(N2, D1, S2),
    Sum is S1 + S2,
    assert(visited(N, Depth, Sum)).

search(N, TargetDepth, Depth):-
    \+ visited(N, Depth, _),
    Depth<TargetDepth,
    N > 0,
    number_codes(N, Codes),
    length(Codes, NumCodes),
    1 is NumCodes rem 2,
    N1 is N * 2024,
    succ(Depth, D1),
    search(N1, TargetDepth, D1),
    visited(N1, D1, Num),
    assert(visited(N, Depth, Num)).


