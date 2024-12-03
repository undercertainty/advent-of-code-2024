
%%% I'm sure that this will all be much easier to solve with python,
%%% but I'm doing some prolog at the moment, and feel like it might
%%% be fun to do a few of this year's tasks in it.

%%% I'm also interested in how far I can use DCGs.

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).
:- use_module(library(yall)).


aoc_01_test_part1(Out):-
    solve_aoc_01_part1("data/data_01_2024_test.txt", Out).

aoc_01_part1(Out):-
    solve_aoc_01_part1("data/data_01_2024.txt", Out).

solve_aoc_01_part1(FileName, Out):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "", "\s\t\n", [StrippedFileString]),
    string_codes(StrippedFileString, StringCodes),
    phrase(s([List1, List2]), StringCodes),
    msort(List1, SortedList1),
    msort(List2, SortedList2),
    maplist([X, Y, Z]>>(Z is abs(X - Y)), SortedList1, SortedList2, Differences),
    sum_list(Differences, Out).



s([[], []]) --> [].

s([[N1|R1], [N2|R2]]) --> line([N1, N2]),
                            eol,
                            s([R1, R2]).

line([N1, N2]) --> number(N1),
         blanks,
         number(N2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:

% Can use the same parser, but a slightly more complicated different reconcile 
% function:

aoc_01_test_part2(Out):-
    solve_aoc_01_part2("data/data_01_2024_test.txt", Out).

aoc_01_part2(Out):-
    solve_aoc_01_part2("data/data_01_2024.txt", Out).

solve_aoc_01_part2(FileName, Out):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "", "\s\t\n", [StrippedFileString]),
    string_codes(StrippedFileString, StringCodes),
    phrase(s([List1, List2]), StringCodes),
    msort(List2, LL2),
    clumped(LL2, FreqList),
    dict_create(FreqDict, freq, FreqList),
    maplist([X, Y]>>(.(FreqDict, get(X, 0), Z), Y is X * Z), List1, Similarities),
    sum_list(Similarities, Out).



