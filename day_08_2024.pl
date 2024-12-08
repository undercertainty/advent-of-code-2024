
%%% 
%%%
%%% Part 1 looks suspiciously straightforward
%%% for a weekend

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

aoc_08_test_part1(Out):-
    solve_aoc_08_part1("data/data_08_2024_test.txt", Out).

aoc_08_part1(Out):-
    solve_aoc_08_part1("data/data_08_2024.txt", Out).

get_file_as_map(FileName, map([Row|Rows], Width, Height)):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "\n", "\n\s\t", MapStrings),
    maplist(string_chars, MapStrings, [Row|Rows]),
    length([Row|Rows], Height),
    length(Row, Width).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff


solve_aoc_08_part1(FileName, Out):-
    get_file_as_map(FileName, Map),
    findall(antinode(X, Y), get_antinode(X, Y, _Antenna, Map), AntiNodes),
    list_to_set(AntiNodes, UniqueAntinodes),
    length(UniqueAntinodes, Out).


get_antenna(X, Y, Antenna, map(Layout, _, _)):-
    nth1(Y, Layout, Row),
    nth1(X, Row, Antenna),
    Antenna \= '.'.

get_antinode(X, Y, Antenna, Map):-
    get_antenna(X1, Y1, Antenna, Map),
    get_antenna(X2, Y2, Antenna, Map),
    [X1, Y1] \= [X2, Y2],
    XDiff is X2-X1,
    YDiff is Y2-Y1,
    X is X1-XDiff,
    Y is Y1-YDiff,
    in_map(X, Y, Map).

get_antinode(X, Y, Antenna, Map, antenna(X1, Y1, Antenna), antenna(X2, Y2, Antenna)):-
    get_antenna(X1, Y1, Antenna, Map),
    get_antenna(X2, Y2, Antenna, Map),
    [X1, Y1] \= [X2, Y2],
    XDiff is X2-X1,
    YDiff is Y2-Y1,
    X is X1-XDiff,
    Y is Y1-YDiff,
    in_map(X, Y, Map).


% makes things slightly tidier:
in_map(X, Y, map(_Layout, Width, Height)):-
    X>=1,
    Y>=1,
    X=<Width,
    Y=<Height.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:

aoc_08_test_part2(Out):-
    solve_aoc_08_part2("data/data_08_2024_test.txt", Out).

aoc_08_part2(Out):-
    solve_aoc_08_part2("data/data_08_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

solve_aoc_08_part2(FileName, Out):-
    get_file_as_map(FileName, Map),
    findall(AntiNodes, get_harmonic_antinodes(Map, AntiNodes), AntiNodes1), 
    flatten(AntiNodes1, AllAntiNodes),
    list_to_set(AllAntiNodes, UniqueAntiNodes),
    length(UniqueAntiNodes, Out).

get_harmonic_antinodes(Map, AntiNodes):-
    get_antenna(X1, Y1, Antenna, Map),
    get_antenna(X2, Y2, Antenna, Map),
    [X1, Y1] \= [X2, Y2],
    XDiff is X2-X1,
    YDiff is Y2-Y1,
    get_harmonic_antinodes(X1, Y1, [], XDiff, YDiff, Map, AntiNodes).


get_harmonic_antinodes(X, Y, AntiNodes, _XDiff, _YDiff, Map, AntiNodes):-
    \+ in_map(X, Y, Map),!.
get_harmonic_antinodes(X, Y, SoFar, XDiff, YDiff, Map, AntiNodes):-
    X1 is X-XDiff,
    Y1 is Y-YDiff,
    get_harmonic_antinodes(X1, Y1, [antinode(X, Y)|SoFar], XDiff, YDiff, Map, AntiNodes).

