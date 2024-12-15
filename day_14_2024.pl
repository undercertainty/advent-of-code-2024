
%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library imports

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AoC map boilerplate

aoc_14_test_part1(Out):-
    solve_aoc_14_part1("data/data_14_2024_test.txt", 11, 7, Out).

aoc_14_part1(Out):-
    solve_aoc_14_part1("data/data_14_2024.txt", 101, 103, Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

solve_aoc_14_part1(FileName, Width, Height, Out):-
    get_inputs(FileName, Robots),
    move_robots(Robots, 100, Width, Height, MovedRobots),
    safety_factor(MovedRobots, Width, Height, Out).

get_inputs(FileName, Inputs):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "", "\n\s\t", [MapStrings]),
    string_codes(MapStrings, Codes),
    phrase(robots(Inputs), Codes).

robots([Robot]) --> robot(Robot).
robots([Robot|Robots]) --> robot(Robot),
                          eol,
                          robots(Robots).

robot(robot(Px, Py, Vx, Vy)) --> "p=",
                                 number(Px), ",", number(Py),
                                 blank,
                                 "v=",
                                 number(Vx), ",", number(Vy).

move_robots(Robots, 0, _, _, Robots).

move_robots(Robots, N, MapWidth, MapHeight, Out):-
    N>0,
    succ(N1, N),
    maplist(move_robot(MapWidth, MapHeight), Robots, MovedRobots),
    move_robots(MovedRobots, N1, MapWidth, MapHeight, Out).

move_robot(MapWidth, MapHeight, robot(Px, Py, Vx, Vy), robot(Px1, Py1, Vx, Vy)):-
    % Hack to make negative moves off the
    % map slightly easier to handle:
    PxTmp is Px + MapWidth + Vx,
    PyTmp is Py + MapHeight + Vy,
    divmod(PxTmp, MapWidth, _, Px1),
    divmod(PyTmp, MapHeight, _, Py1).

safety_factor(MovedRobots, Width, Height, Out):-
    XMid is Width//2,
    YMid is Height//2,

    % Get top left robots
    findall([X, Y], (member(robot(X, Y, _, _), MovedRobots), X < XMid, Y < YMid), TLRobots),
    length(TLRobots, TL),

    % Get top right robots
    findall([X, Y], (member(robot(X, Y, _, _), MovedRobots), X > XMid, Y < YMid), TRRobots),
    length(TRRobots, TR),

    % Get bottom left robots
    findall([X, Y], (member(robot(X, Y, _, _), MovedRobots), X < XMid, Y > YMid), BLRobots),
    length(BLRobots, BL),

    % Get bottom right robots
    findall([X, Y], (member(robot(X, Y, _, _), MovedRobots), X > XMid, Y > YMid), BRRobots),
    length(BRRobots, BR),

    Out is TR * TL * BR * BL.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:
%
%
% How the f*** are you supposed to do this?
%
% OK... let's see what happens if we find a state where there's
% an unbroken row of 10 robots. That might be an image..?

% Shouldn't think this will work...
aoc_14_test_part2(Out):-
    solve_aoc_14_part2("data/data_14_2024_test.txt", Out).

% ... so just do the main case.
aoc_14_part2(Out):-
    solve_aoc_14_part2("data/data_14_2024.txt", 10, 101, 103, Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

solve_aoc_14_part2(FileName, RowLength, Width, Height, Out):-
    get_inputs(FileName, Robots),
    find_unbroken_row(Robots, RowLength, Width, Height, Out).



unbroken_row(Robots, RowLength):-
    succ(L, RowLength), % indexing hack
    member(robot(XLeft, Y, _, _), Robots),
    XRight is XLeft+L,
    \+ (between(XLeft, XRight, X),
        \+ member(robot(X, Y, _, _), Robots)).


find_unbroken_row(Robots, RowLength, Width, Height, Out):-
    find_unbroken_row(Robots, RowLength, Width, Height, 0, Out).

find_unbroken_row(Robots, RowLength, _, _, Out, Out):-
    unbroken_row(Robots, RowLength).
find_unbroken_row(Robots, RowLength, MapWidth, MapHeight, N, Out):-
    succ(N, N1),
    (divmod(N1, 1000, _, 0) -> (print(N1), nl) ; true),
    maplist(move_robot(MapWidth, MapHeight), Robots, MovedRobots),
    find_unbroken_row(MovedRobots, RowLength, MapWidth, MapHeight, N1, Out).



% Ha ha! Yes, it worked!!