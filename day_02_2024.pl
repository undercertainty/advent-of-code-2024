
%%% I'm sure that this will all be much easier to solve with python,
%%% but I'm doing some prolog at the moment, and feel like it might
%%% be fun to do a few of this year's tasks in it.

%%% I'm also interested in how far I can use DCGs.

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).


aoc_02_test_part1(Out):-
    solve_aoc_02_part1("data/data_02_2024_test.txt", Out).

aoc_02_part1(Out):-
    solve_aoc_02_part1("data/data_02_2024.txt", Out).


solve_aoc_02_part1(FileName, Out):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "", "\s\t\n", [StrippedFileString]),
    string_codes(StrippedFileString, Codes),
    phrase(s(Reports), Codes),
    include(safe_report, Reports, SafeReports),
    length(SafeReports, Out).

s([Report]) --> line(Report).

s([Report|Reports]) --> line(Report),
                            eol,
                            s(Reports).

line([N]) --> number(N).

line([N|Rest]) --> number(N),
         whites,
         line(Rest).

% Reports are safe if adjacent numbers are all between 1 and 3 inclusive

safe_report([ReportHead|ReportBack]):-
    reverse([ReportHead|ReportBack], [_|Lr]),
    reverse(Lr, ReportFront),
    maplist([X, Y, Z]>>(Z is X-Y), ReportFront, ReportBack, Differences),
    (
        exclude(call(between(1, 3)), Differences, [])
        ;
        exclude(call(between(-3, -1)), Differences, [])
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:

% Oh ho, the nondeterministic nature of prolog should actually
% make this fairly straightforward...

aoc_02_test_part2(Out):-
    solve_aoc_02_part2("data/data_02_2024_test.txt", Out).

aoc_02_part2(Out):-
    solve_aoc_02_part2("data/data_02_2024.txt", Out).


solve_aoc_02_part2(FileName, Out):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "", "\s\t\n", [StrippedFileString]),
    string_codes(StrippedFileString, Codes),
    phrase(s(Reports), Codes),
    include(safe_report_2, Reports, SafeReports),
    length(SafeReports, Out).


safe_report_2(Report):-
    safe_report(Report).

safe_report_2(Report):-
    select(_, Report, DampedReport),
    safe_report(DampedReport).
