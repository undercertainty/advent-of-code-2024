
%%% I'm sure that this will all be much easier to solve with python,
%%% but I'm doing some prolog at the moment, and feel like it might
%%% be fun to do a few of this year's tasks in it.

%%% I'm also interested in how far I can use DCGs.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library imports

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AoC boilerplate

aoc_03_test_part1(Out):-
    solve_aoc_03_part1("data/data_03_2024_test.txt", Out).

aoc_03_part1(Out):-
    solve_aoc_03_part1("data/data_03_2024.txt", Out).

get_file_as_codes(FileName, Codes):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "", "\s\t\n", [StrippedFileString]),
    string_codes(StrippedFileString, Codes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

solve_aoc_03_part1(FileName, Out):-
    get_file_as_codes(FileName, Codes),
    phrase(s(Out), Codes).

% Handle everything in the DCG. That's cool...

s(Total) --> string(_Codes),
                  mul(Mul),
                  s(Rest),
                  {Total is Mul + Rest}.

s(0) --> string(_Codes).

mul(Mul) --> "mul(",
                  number(N1),
                  ",",
                  number(N2),
                  ")",
                  {Mul is N1 * N2}.
                


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:

% Looks as though I'm just wanting to improve the DCG
%
% Little simplifying hack: I'll just append the codes for
% "do()" to the beginning of the input to set things up
% for being initially enabled.

aoc_03_test_part2(Out):-
    solve_aoc_03_part2("data/data_03_2024_testb.txt", Out).

aoc_03_part2(Out):-
    solve_aoc_03_part2("data/data_03_2024.txt", Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

solve_aoc_03_part2(FileName, Out):-
    get_file_as_codes(FileName, Codes),
    phrase(do_block(Out), Codes).

do_block(Out) --> string(_Codes),
                  do_block_action(Out).

do_block(0) --> string(_Codes).

dont_block(Out) --> string(_Codes),
                    "do()",
                    do_block(Out).

dont_block(0)--> string(_Codes).

do_block_action(Out) --> mul(Mul),
                         do_block(Rest),
                         {Out is Mul+Rest}.

do_block_action(Out) --> "don't()",
                         dont_block(Out).

