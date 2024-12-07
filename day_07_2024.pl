
%%% 
%%%
%%% Part 1 looks suspiciously straightforward
%%% for a weekend

%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library imports

:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AoC boilerplate

aoc_07_test_part1(Out):-
    solve_aoc_07_part1("data/data_07_2024_test.txt", Out).

aoc_07_part1(Out):-
    solve_aoc_07_part1("data/data_07_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff


solve_aoc_07_part1(FileName, Out):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "\n", "\n\s\t", Equations),
    true_equations(Equations, EquationValues),
    sumlist(EquationValues, Out).

true_equations([], []).
true_equations([Equation|Rest], Out):-
    true_equations(Rest, RestOut),
    (true_equation(Equation, Value)
        -> Out = [Value|RestOut]
         ; Out = RestOut).


true_equation(String, Target):-
    string_codes(String, Codes),
    phrase(eq(Target, Values), TargetCodes),
    true_equation1(Values, Target).

true_equation1([N], N).
true_equation1([N1, N2|R], Out):-
    N is N1 + N2,
    true_equation1([N|R], Out).
true_equation1([N1, N2|R], Out):-
    N is N1 * N2,
    true_equation1([N|R], Out).


eq(Target, Values) --> number(Target),
                 ":",
                 blanks,
                 values(Values).

values([]) --> [].
values([Val|Vals]) -->  number(Val),
                        blanks,
                        values(Vals).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:

aoc_07_test_part2(Out):-
    solve_aoc_07_part2("data/data_07_2024_test.txt", Out).

aoc_07_part2(Out):-
    solve_aoc_07_part2("data/data_07_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

solve_aoc_07_part2(FileName, Out):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "\n", "\n\s\t", Equations),
    true_equations2(Equations, EquationValues),
    sumlist(EquationValues, Out).

concatenate_numbers(Num1, Num2, NumOut):-
    number_codes(Num1, Codes1),
    number_codes(Num2, Codes2),
    append(Codes1, Codes2, CodesOut),
    number_codes(NumOut, CodesOut).

true_equations2([], []).
true_equations2([Equation|Rest], Out):-
    true_equations2(Rest, RestOut),
    (true_equation2(Equation, Value)
        -> Out = [Value|RestOut]
         ; Out = RestOut).


true_equation2(String, Target):-
    string_codes(String, Codes),
    phrase(eq(Target, Values), Codes),
    true_equation2a(Values, Target).


true_equation2a([N], N).
true_equation2a([N1, N2|R], Out):-
    N is N1 + N2,
    true_equation2a([N|R], Out).
true_equation2a([N1, N2|R], Out):-
    N is N1 * N2,
    true_equation2a([N|R], Out).
true_equation2a([N1, N2|R], Out):-
    concatenate_numbers(N1, N2, N),
    true_equation2a([N|R], Out).

