
%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library imports

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(yall)).
:- use_module(library(dcg/basics)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AoC map boilerplate

aoc_15_test_part1(Out):-
    solve_aoc_15_part1("data/data_15_2024_test.txt", Out).

aoc_15_part1(Out):-
    solve_aoc_15_part1("data/data_15_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

solve_aoc_15_part1(FileName, Out):-
    get_inputs(FileName, Inputs),!,
    follow_moves(Inputs, FinalState),
    final_score(FinalState, Out).

get_inputs(FileName, day_15(Grid, Moves)):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "", "\n\s\t", [MapStrings]),
    string_codes(MapStrings, Codes),
    phrase(day_15(Grid, Moves), Codes).


day_15(Grid, Moves) --> grid(Grid),
                        eol,
                        eol,
                        moves(Moves).

grid([RowChars]) --> nonblanks(Row),
                     {maplist([X, Y]>>char_code(Y, X), Row, RowChars)}.
grid([RowChars|Rows]) --> nonblanks(Row),
                     eol,
                     grid(Rows),
                     {maplist([X, Y]>>char_code(Y, X), Row, RowChars)}.

moves(Moves) --> nonblanks(MoveCodes),
                 {maplist([X, Y]>>char_code(Y, X), MoveCodes, Moves)}.
moves(Moves) --> nonblanks(MoveCodes),
                 eol,
                 moves(RestOfMoves),
                 {maplist([X, Y]>>char_code(Y, X), MoveCodes, MoveChars),
                  append(MoveChars, RestOfMoves, Moves)}.

% Fails if robot cannot move
move_robot_right(GridIn, GridOut):-
    append(RowsAbove, [RowWithRobot|RowsBelow], GridIn),
    append(TilesLeft, ['@'|TilesRight], RowWithRobot),
    append(Boxes, ['.'|Tiles], TilesRight),
    \+ member('.', Boxes),
    \+ member('#', Boxes),
    append(['.', '@'|Boxes], Tiles, TilesRightUpdated),
    append(TilesLeft, TilesRightUpdated, RowUpdated),
    append(RowsAbove, [RowUpdated|RowsBelow], GridOut).

move_robot_up(GridIn, GridOut):-
    rotate_clockwise(GridIn, Grid1),
    move_robot_right(Grid1, GridMoved),
    rotate_clockwise(GridMoved, GridMoved1),
    rotate_clockwise(GridMoved1, GridMoved2),
    rotate_clockwise(GridMoved2, GridOut).

move_robot_left(GridIn, GridOut):-
    rotate_clockwise(GridIn, Grid1),
    rotate_clockwise(Grid1, Grid2),
    move_robot_right(Grid2, GridMoved),
    rotate_clockwise(GridMoved, GridMoved3),
    rotate_clockwise(GridMoved3, GridOut).

move_robot_down(GridIn, GridOut):-
    rotate_clockwise(GridIn, Grid1),
    rotate_clockwise(Grid1, Grid2),
    rotate_clockwise(Grid2, Grid3),
    move_robot_right(Grid3, GridMoved),
    rotate_clockwise(GridMoved, GridOut).


follow_moves(day_15(Grid, Moves), Out):-
    follow_moves(Grid, Moves, Out).

% Cuts to stop a stack overflow; should be
% deterministic.

follow_moves(Grid, [], Grid).
follow_moves(GridIn, ['^'|Moves], Out):-
    !,
    (move_robot_up(GridIn, GridMoved)
    ->
    follow_moves(GridMoved, Moves, Out)
    ;
    follow_moves(GridIn, Moves, Out)).

follow_moves(GridIn, ['v'|Moves], Out):-
    !,
    (move_robot_down(GridIn, GridMoved)
    ->
    follow_moves(GridMoved, Moves, Out)
    ;
    follow_moves(GridIn, Moves, Out)).

follow_moves(GridIn, ['<'|Moves], Out):-
    !,
    (move_robot_left(GridIn, GridMoved)
    ->
    follow_moves(GridMoved, Moves, Out)
    ;
    follow_moves(GridIn, Moves, Out)).

follow_moves(GridIn, ['>'|Moves], Out):-
    !,
    (move_robot_right(GridIn, GridMoved)
    ->
    follow_moves(GridMoved, Moves, Out)
    ;
    follow_moves(GridIn, Moves, Out)).



final_score(Grid, Out):-
    findall(GPS, (nth0(Y, Grid, Row), nth0(X, Row, 'O'), GPS is X + 100*Y), GpsScores),
    sum_list(GpsScores, Out).

    
    
    
rotate_clockwise([[]|_EmptyColumns], []).
rotate_clockwise(GridIn, [RevCol|GridOut]):-
    get_column(GridIn, Column, GridRest),
    rotate_clockwise(GridRest, GridOut),
    reverse(Column, RevCol).


get_column([], [], []).
get_column([[C|Row]|Rows], [C|Column], [Row|RowsOut]):-
    get_column(Rows, Column, RowsOut).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:
%
%
% God, this looks a bit fiddly...
%

aoc_15_test_part2(Out):-
    solve_aoc_15_part2("data/data_15_2024_test.txt", Out).


aoc_15_part2(Out):-
    solve_aoc_15_part2("data/data_15_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

solve_aoc_15_part2(FileName, Out):-
    get_inputs2(FileName, Grid, Moves),!,
    follow_moves2(Grid, Moves, FinalState),
    %Out=FinalState.
    final_score2(FinalState, Out).

get_inputs2(FileName, TransformedGrid, Moves):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "", "\n\s\t", [MapStrings]),
    string_codes(MapStrings, Codes),
    phrase(day_15(Grid, Moves), Codes),
    transform_map(Grid, TransformedGrid).

transform_map([], []).
transform_map([Row|Rows], [NewRow|NewRows]):-
    transform_row(Row, NewRow),
    transform_map(Rows, NewRows).

transform_row([], []).
transform_row(['#'|Row], ['#', '#'|NewRow]):-
    transform_row(Row, NewRow).
transform_row(['.'|Row], ['.', '.'|NewRow]):-
    transform_row(Row, NewRow).
transform_row(['O'|Row], ['[', ']'|NewRow]):-
    transform_row(Row, NewRow).
transform_row(['@'|Row], ['@', '.'|NewRow]):-
    transform_row(Row, NewRow).


% OK... let's try to move objects one at a time.
% I'm going to try to move the object at location
% X, Y by one square.

% If the next spot is a gap, move the object into the gap

move_object_right(X, Y, GridIn, GridOut):-
    append(RowsAbove, [RowWithObject|RowsBelow], GridIn),
    length(RowsAbove, Y),
    append(TilesLeft, [Object, '.'|TilesRight], RowWithObject),
    length(TilesLeft, X),
    append(TilesLeft, ['.', Object|TilesRight], NewRow),
    append(RowsAbove, [NewRow|RowsBelow], GridOut).

% If the next spot is part of a box, move the object into the gap
% Lots of redundancy, but get it right first...
move_object_right(X, Y, GridIn, GridOut):-
    append(RowsAbove, [RowWithObject|_RowsBelow], GridIn),
    length(RowsAbove, Y),
    append(TilesLeft, [_Object, '['|_TilesRight], RowWithObject),
    length(TilesLeft, X),
    succ(X, X1),
    move_object_right(X1, Y, GridIn, UpdatedGrid),
    % Should now be clear to move the original object
    move_object_right(X, Y, UpdatedGrid, GridOut).
move_object_right(X, Y, GridIn, GridOut):-
    append(RowsAbove, [RowWithObject|_RowsBelow], GridIn),
    length(RowsAbove, Y),
    append(TilesLeft, [_Object, ']'|_TilesRight], RowWithObject),
    length(TilesLeft, X),
    succ(X, X1),
    move_object_right(X1, Y, GridIn, UpdatedGrid),
    % Should now be clear to move the original object
    move_object_right(X, Y, UpdatedGrid, GridOut).



% Now do a similar thing, but we're considering double moves.
% Fortunately, all the boxes are oriented in the same direction.
%
% I'm going to try to move the object at location
% X, Y by one square.

% If the next spot is a gap, move the object into the gap

move_double_object_up(X, Y, GridIn, GridOut):-
    append(RowsAbove, [RowWithObject|RowsBelow], GridIn),
    length(RowsAbove, Y),
    append(TilesLeft, [Object, '.'|TilesRight], RowWithObject),
    length(TilesLeft, X),
    append(TilesLeft, ['.', Object|TilesRight], NewRow),
    append(RowsAbove, [NewRow|RowsBelow], GridOut).

% Otherwise, need to move both parts of the block

move_double_object_up(X, Y, GridIn, GridOut):-
    append(RowsAbove, [RowWithObject|_RowsBelow], GridIn),
    length(RowsAbove, Y),
    append(TilesLeft, [_Object, '['|_TilesRight], RowWithObject),
    length(TilesLeft, X),
    succ(X, X1),
    succ(Y, Y1),
    % Move where the robot is pushing
    move_double_object_up(X1, Y, GridIn, UpdatedGrid),
    % And the rest of the box
    move_double_object_up(X1, Y1, UpdatedGrid, UpdatedGrid2),
    % Then the original object
    move_object_right(X, Y, UpdatedGrid2, GridOut).

move_double_object_up(X, Y, GridIn, GridOut):-
    append(RowsAbove, [RowWithObject|_RowsBelow], GridIn),
    length(RowsAbove, Y),
    append(TilesLeft, [_Object, ']'|_TilesRight], RowWithObject),
    length(TilesLeft, X),
    succ(X, X1),
    succ(Y1, Y),
    % Move where the robot is pushing
    move_double_object_up(X1, Y, GridIn, UpdatedGrid),
    % And the rest of the box
    move_double_object_up(X1, Y1, UpdatedGrid, UpdatedGrid2),
    % Then the original object
    move_object_right(X, Y, UpdatedGrid2, GridOut).


% Need another set of predicates for when we're moving
% double objects down:

move_double_object_down(X, Y, GridIn, GridOut):-
    append(RowsAbove, [RowWithObject|RowsBelow], GridIn),
    length(RowsAbove, Y),
    append(TilesLeft, [Object, '.'|TilesRight], RowWithObject),
    length(TilesLeft, X),
    append(TilesLeft, ['.', Object|TilesRight], NewRow),
    append(RowsAbove, [NewRow|RowsBelow], GridOut).

% Otherwise, need to move both parts of the block

move_double_object_down(X, Y, GridIn, GridOut):-
    append(RowsAbove, [RowWithObject|_RowsBelow], GridIn),
    length(RowsAbove, Y),
    append(TilesLeft, [_Object, '['|_TilesRight], RowWithObject),
    length(TilesLeft, X),
    succ(X, X1),
    succ(Y1, Y),
    % Move where the robot is pushing
    move_double_object_down(X1, Y, GridIn, UpdatedGrid),
    % And the rest of the box
    move_double_object_down(X1, Y1, UpdatedGrid, UpdatedGrid2),
    % Then the original object
    move_object_right(X, Y, UpdatedGrid2, GridOut).

move_double_object_down(X, Y, GridIn, GridOut):-
    append(RowsAbove, [RowWithObject|_RowsBelow], GridIn),
    length(RowsAbove, Y),
    append(TilesLeft, [_Object, ']'|_TilesRight], RowWithObject),
    length(TilesLeft, X),
    succ(X, X1),
    succ(Y, Y1),
    % Move where the robot is pushing
    move_double_object_down(X1, Y, GridIn, UpdatedGrid),
    % And the rest of the box
    move_double_object_down(X1, Y1, UpdatedGrid, UpdatedGrid2),
    % Then the original object
    move_object_right(X, Y, UpdatedGrid2, GridOut).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
move_robot_right2(GridIn, GridOut):-
    find_robot(GridIn, X, Y),
    move_object_right(X, Y, GridIn, GridOut).

move_robot_up2(GridIn, GridOut):-
    rotate_clockwise(GridIn, Grid1),
    find_robot(Grid1, X, Y),
    move_double_object_up(X, Y, Grid1, GridMoved),
    rotate_clockwise(GridMoved, GridMoved1),
    rotate_clockwise(GridMoved1, GridMoved2),
    rotate_clockwise(GridMoved2, GridOut).

move_robot_left2(GridIn, GridOut):-
    rotate_clockwise(GridIn, Grid1),
    rotate_clockwise(Grid1, Grid2),
    find_robot(Grid2, X, Y),
    move_object_right(X, Y, Grid2, GridMoved),
    rotate_clockwise(GridMoved, GridMoved3),
    rotate_clockwise(GridMoved3, GridOut).

move_robot_down2(GridIn, GridOut):-
    rotate_clockwise(GridIn, Grid1),
    rotate_clockwise(Grid1, Grid2),
    rotate_clockwise(Grid2, Grid3),
    find_robot(Grid3, X, Y),
    move_double_object_down(X, Y, Grid3, GridMoved),
    rotate_clockwise(GridMoved, GridOut).



find_robot(Grid, X, Y):-
    nth0(Y, Grid, Row),
    nth0(X, Row, '@').


follow_moves2(Grid, [], Grid).
follow_moves2(GridIn, ['^'|Moves], Out):-
    !,
    (move_robot_up2(GridIn, GridMoved)
    ->
    follow_moves2(GridMoved, Moves, Out)
    ;
    follow_moves2(GridIn, Moves, Out)).

follow_moves2(GridIn, ['v'|Moves], Out):-
    !,
    (move_robot_down2(GridIn, GridMoved)
    ->
    follow_moves2(GridMoved, Moves, Out)
    ;
    follow_moves2(GridIn, Moves, Out)).

follow_moves2(GridIn, ['<'|Moves], Out):-
    !,
    (move_robot_left2(GridIn, GridMoved)
    ->
    follow_moves2(GridMoved, Moves, Out)
    ;
    follow_moves2(GridIn, Moves, Out)).

follow_moves2(GridIn, ['>'|Moves], Out):-
    !,
    (move_robot_right2(GridIn, GridMoved)
    ->
    follow_moves2(GridMoved, Moves, Out)
    ;
    follow_moves2(GridIn, Moves, Out)).


final_score2(Grid, Out):-
    findall(GPS, (nth0(Y, Grid, Row), nth0(X, Row, '['), GPS is X + 100*Y), GpsScores),
    sum_list(GpsScores, Out).


/*

portray([]).
portray([X|R]):-
    is_list(X),
    nl,
    write(X),
    portray(R).
*/