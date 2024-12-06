
%%% 
%%%
%%% Too slow using lists, so I'm using assert and retract
%%% for the speed gains.


%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library imports

:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(readutil)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AoC boilerplate

aoc_06_test_part1(Out):-
    solve_aoc_06_part1("data/data_06_2024_test.txt", Out).

aoc_06_part1(Out):-
    solve_aoc_06_part1("data/data_06_2024.txt", Out).

get_file_as_grid(FileName, Grid):-
    read_file_to_string(FileName, FileString, []),
    string_lower(FileString, StringLower),
    split_string(StringLower, "\n", "\n\s\t", Strings),
    maplist(string_chars, Strings, Grid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff



solve_aoc_06_part1(FileName, Out):-
    get_file_as_grid(FileName, Map),
    get_initial_state(Map, InitialState),
    build_path(InitialState),
    findall([X, Y], guard(_, X, Y), Locs),
    list_to_set(Locs, UniqueLocs),
    length(UniqueLocs, Out),
    retractall(guard(_, _, _)).


% Let's keep it simple and just use a structure
% state(Guard, Map) rather than fiddle around with
% dicts or the like for the time being.

get_initial_state(MapIn, state(guard('^', X, Y),
                            map(MapOut, Width, Height))):-
    nth1(Y, MapIn, Row),
    nth1(X, Row, '^'),
    select('^', Row, '.', RowOut),
    select(Row, MapIn, RowOut, MapOut),
    length(Row, Width),
    length(MapIn, Height).


% Slightly ease things by having a predicate to read
% from the map.

read_map(X, Y, map(Map, _Width, _Height), space):-
    nth1(Y, Map, Row),
    nth1(X, Row, '.').
read_map(X, Y, map(Map, _Width, _Height), obstruction):-
    nth1(Y, Map, Row),
    nth1(X, Row, '#').

% And want the next state when the guard moves:

% Do the states where the guard leaves the area
next_state(state(guard('^', _, 1), _Map), guard_leaves).
next_state(state(guard('V', _, MapHeight),
                 map(_, _, MapHeight)), guard_leaves).
next_state(state(guard('<', 1, _), _Map), guard_leaves).
next_state(state(guard('>', MapWidth, _),
                 map(_, MapWidth, _)), guard_leaves).

% Do the states where the guard moves
next_state(state(guard('^', X, Y), Map),
           state(guard('^', X, Y1), Map)):-
    Y1 is Y-1,
    read_map(X, Y1, Map, space).
next_state(state(guard('V', X, Y), Map),
           state(guard('V', X, Y1), Map)):-
    Y1 is Y+1,
    read_map(X, Y1, Map, space).
next_state(state(guard('<', X, Y), Map),
           state(guard('<', X1, Y), Map)):-
    X1 is X-1,
    read_map(X1, Y, Map, space).
next_state(state(guard('>', X, Y), Map),
           state(guard('>', X1, Y), Map)):-
    X1 is X+1,
    read_map(X1, Y, Map, space).


% And the states where the guard turns:

next_state(state(guard('^', X, Y), Map),
           state(guard('>', X, Y), Map)):-
    Y1 is Y-1,
    read_map(X, Y1, Map, obstruction).
next_state(state(guard('V', X, Y), Map),
           state(guard('<', X, Y), Map)):-
    Y1 is Y+1,
    read_map(X, Y1, Map, obstruction).
next_state(state(guard('<', X, Y), Map),
           state(guard('^', X, Y), Map)):-
    X1 is X-1,
    read_map(X1, Y, Map, obstruction).
next_state(state(guard('>', X, Y), Map),
           state(guard('V', X, Y), Map)):-
    X1 is X+1,
    read_map(X1, Y, Map, obstruction).


build_path(state(Guard, Map)):-
    next_state(state(Guard, Map), guard_leaves),
    assert(Guard).
build_path(state(GuardIn, Map)):-
    next_state(state(GuardIn, Map), state(GuardOut, Map)),
    assert(GuardIn),
    build_path(state(GuardOut, Map)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:

% This will not be fast, but I'm happy to leave it
% running overnight :-).


aoc_06_test_part2(Out):-
    solve_aoc_06_part2("data/data_06_2024_test.txt", Out).

aoc_06_part2(Out):-
    solve_aoc_06_part2("data/data_06_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff


solve_aoc_06_part2(FileName, Out):-
    get_file_as_grid(FileName, Map),
    get_initial_state(Map, InitialState),
    findall([X, Y], (update_map(InitialState, X, Y, '.', '#', UpdatedState),
                     identify_loop(UpdatedState, loops)),
            Blockers),
    length(Blockers, Out).


% Be helpful to have a predicate to update
% the map. I'll do it from a state:

update_map(state(Guard, map(MapIn, W, H)),
           X, Y, OldValue, NewValue, 
           state(Guard, map(MapOut, W, H))):-
    append(RowsBefore, [Row|RowsAfter], MapIn),
    length(RowsBefore, Y),
    append(TilesBefore, [OldValue|TilesAfter], Row),
    length(TilesBefore, X),
    append(TilesBefore, [NewValue|TilesAfter], NewRow),
    append(RowsBefore, [NewRow|RowsAfter], MapOut).



% Use an accumulator to get the path.
%
% In this case, can just dump the path once we know
% what happens at the end, and return the result

identify_loop(State, Result):-
    retractall(guard(_, _, _)),
    identify_loop1(State, Result).

identify_loop1(State, leaves):-
    next_state(State, guard_leaves).
identify_loop1(state(Guard, _Map), loops):-
    Guard.
identify_loop1(state(GuardIn, Map), Result):-
    next_state(state(GuardIn, Map), state(GuardOut, Map)),
    \+ GuardIn,
    assert(GuardIn),
    identify_loop1(state(GuardOut, Map), Result).



