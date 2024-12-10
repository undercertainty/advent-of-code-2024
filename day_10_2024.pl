
%%% 
%%% Another map searcher. Nice of Eric to put in
%%% lots of prolog-friendly tasks this year.
%%% 

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

aoc_10_test_part1(Out):-
    solve_aoc_10_part1("data/data_10_2024_test.txt", Out).

aoc_10_part1(Out):-
    solve_aoc_10_part1("data/data_10_2024.txt", Out).


get_file_as_map(FileName, map([Row|Rows], Width, Height)):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "\n", "\n\s\t", MapStrings),
    maplist([X, Y]>>(string_chars(X, Z), 
                     maplist(atom_number, Z, Y)), MapStrings, [Row|Rows]),
    length([Row|Rows], Height),
    length(Row, Width).

%%% Fail if loc not in the map

get_map_value(X, Y, map(Map, _, _), Val):-
    nth1(Y, Map, Row),
    nth1(X, Row, Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff


solve_aoc_10_part1(FileName, Out):-
    get_file_as_map(FileName, Map),
    findall(Path, path(Map, Path), Paths),
    maplist([[Start|Path], [Start, Finish]]>>last(Path, Finish),
            Paths, StartDests),
    list_to_set(StartDests, UniqueStartDests),
    length(UniqueStartDests, Out).
    

% Possibly overchecking the heights, but
% that's not going to be the bottleneck
next_trail_step(X, Y, X1, Y1, Map):-
    get_map_value(X, Y, Map, Height),
    next_trail_step(X, Y, X1, Y1, Height, Map).

next_trail_step(X, Y, X1, Y, CurrentHeight, Map):-
    succ(X, X1),
    get_map_value(X1, Y, Map, NextHeight),
    succ(CurrentHeight, NextHeight).
next_trail_step(X, Y, X1, Y, CurrentHeight, Map):-
    succ(X1, X),
    get_map_value(X1, Y, Map, NextHeight),
    succ(CurrentHeight, NextHeight).
next_trail_step(X, Y, X, Y1, CurrentHeight, Map):-
    succ(Y, Y1),
    get_map_value(X, Y1, Map, NextHeight),
    succ(CurrentHeight, NextHeight).
next_trail_step(X, Y, X, Y1, CurrentHeight, Map):-
    succ(Y1, Y),
    get_map_value(X, Y1, Map, NextHeight),
    succ(CurrentHeight, NextHeight).

path(Map, Out):-
    get_map_value(X, Y, Map, 0),
    find_trail(X, Y, Map, Out).

find_trail(X, Y, Map, Out):-
    find_trail(X, Y, Map, [], RevTrail),
    reverse(RevTrail, Out).

find_trail(X, Y, Map, Out, [[X, Y]|Out]):-
    get_map_value(X, Y, Map, 9).
find_trail(X, Y, Map, SoFar, Out):-
    next_trail_step(X, Y, X1, Y1, Map),
    find_trail(X1, Y1, Map, [[X, Y]|SoFar], Out).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:

aoc_10_test_part2(Out):-
    solve_aoc_10_part2("data/data_10_2024_test.txt", Out).

aoc_10_part2(Out):-
    solve_aoc_10_part2("data/data_10_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Part 2's easy now: just part 1 without the filter:

solve_aoc_10_part2(FileName, Out):-
    get_file_as_map(FileName, Map),
    findall(Path, path(Map, Path), Paths),
    length(Paths, Out).