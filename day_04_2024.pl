


%%% Not using DCGs today... problem's straightforward enough, but
%%% lots of copying and pasting.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library imports

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AoC boilerplate

aoc_04_test_part1(Out):-
    solve_aoc_04_part1("data/data_04_2024_test.txt", Out).

aoc_04_part1(Out):-
    solve_aoc_04_part1("data/data_04_2024.txt", Out).

get_file_as_grid(FileName, Grid):-
    read_file_to_string(FileName, FileString, []),
    string_lower(FileString, StringLower),
    split_string(StringLower, "\n", "\n\s\t", Strings),
    maplist(string_chars, Strings, Grid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

solve_aoc_04_part1(FileName, Out):-
    get_file_as_grid(FileName, Grid),
    findall(_, contains_xmas(Grid), Solutions),
    length(Solutions, Out).


% Want to be able to rotate the grid by 90deg... can
% do that by transpose and reverse the rows. Have
% transpose/2 built into the FD library.

rotate_grid(GridIn, GridOut):-
    transpose(GridIn, Transposed),
    maplist(reverse, Transposed, GridOut).

% So now we should be able to find all the non-diagonal
% occurrences of XMAS in the grid.

contains_xmas(Grid):-
    member(Row, Grid),
    append(_StartList, [x, m, a, s|_Rest], Row).

contains_xmas(Grid):-
    rotate_grid(Grid, RotatedGrid_90),
    member(Row, RotatedGrid_90),
    append(_StartList, [x, m, a, s|_Rest], Row).

contains_xmas(Grid):-
    rotate_grid(Grid, RotatedGrid_90),
    rotate_grid(RotatedGrid_90, RotatedGrid_180),
    member(Row, RotatedGrid_180),
    append(_StartList, [x, m, a, s|_Rest], Row).

contains_xmas(Grid):-
    rotate_grid(Grid, RotatedGrid_90),
    rotate_grid(RotatedGrid_90, RotatedGrid_180),
    rotate_grid(RotatedGrid_180, RotatedGrid_270),
    member(Row, RotatedGrid_270),
    append(_StartList, [x, m, a, s|_Rest], Row).


% Diagonal will be a little fiddlier... let's assume we've
% got four rows in which we want to find the diagonal:

contains_xmas(Grid):-
    append(_Top, [Row1, Row2, Row3, Row4|_Rest], Grid),
    block_contains_xmas([Row1, Row2, Row3, Row4]).

contains_xmas(Grid):-
    rotate_grid(Grid, RotatedGrid_90),
    append(_Top, [Row1, Row2, Row3, Row4|_Rest], RotatedGrid_90),
    block_contains_xmas([Row1, Row2, Row3, Row4]).

contains_xmas(Grid):-
    rotate_grid(Grid, RotatedGrid_90),
    rotate_grid(RotatedGrid_90, RotatedGrid_180),
    append(_Top, [Row1, Row2, Row3, Row4|_Rest], RotatedGrid_180),
    block_contains_xmas([Row1, Row2, Row3, Row4]).

contains_xmas(Grid):-
    rotate_grid(Grid, RotatedGrid_90),
    rotate_grid(RotatedGrid_90, RotatedGrid_180),
    rotate_grid(RotatedGrid_180, RotatedGrid_270),
    append(_Top, [Row1, Row2, Row3, Row4|_Rest], RotatedGrid_270),
    block_contains_xmas([Row1, Row2, Row3, Row4]).


block_contains_xmas([[x|_Rest1],
                     [_, m|_Rest2],
                     [_, _, a|_Rest3],
                     [_, _, _, s|_Rest4]]).
block_contains_xmas([[_|Row1], [_|Row2], [_|Row3], [_|Row4]]):-
    block_contains_xmas([Row1, Row2, Row3, Row4]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:

% Same as part 1, but actually slightly easier
%

aoc_04_test_part2(Out):-
    solve_aoc_04_part2("data/data_04_2024_test.txt", Out).

aoc_04_part2(Out):-
    solve_aoc_04_part2("data/data_04_2024.txt", Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

solve_aoc_04_part2(FileName, Out):-
    get_file_as_grid(FileName, Grid),
    findall(_, contains_x_mas(Grid), Solutions),
    length(Solutions, Out).

contains_x_mas(Grid):-
    append(_Top, [Row1, Row2, Row3|_Rest], Grid),
    block_contains_x_mas([Row1, Row2, Row3]).

contains_x_mas(Grid):-
    rotate_grid(Grid, RotatedGrid_90),
    append(_Top, [Row1, Row2, Row3|_Rest], RotatedGrid_90),
    block_contains_x_mas([Row1, Row2, Row3]).

contains_x_mas(Grid):-
    rotate_grid(Grid, RotatedGrid_90),
    rotate_grid(RotatedGrid_90, RotatedGrid_180),
    append(_Top, [Row1, Row2, Row3|_Rest], RotatedGrid_180),
    block_contains_x_mas([Row1, Row2, Row3]).

contains_x_mas(Grid):-
    rotate_grid(Grid, RotatedGrid_90),
    rotate_grid(RotatedGrid_90, RotatedGrid_180),
    rotate_grid(RotatedGrid_180, RotatedGrid_270),
    append(_Top, [Row1, Row2, Row3|_Rest], RotatedGrid_270),
    block_contains_x_mas([Row1, Row2, Row3]).


block_contains_x_mas([[m, _, s|_Rest1],
                      [_, a, _|_Rest2],
                      [m, _, s|_Rest3]]).
block_contains_x_mas([[_|Row1], [_|Row2], [_|Row3]]):-
    block_contains_x_mas([Row1, Row2, Row3]).


