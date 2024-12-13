
% At last! A finite domains problem...
%
% ... (having done part 2) Bah, it's not a
% FD problem, it's just two lines crossing >:-|

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

aoc_13_test_part1(Out):-
    solve_aoc_13_part1("data/data_13_2024_test.txt", Out).

aoc_13_part1(Out):-
    solve_aoc_13_part1("data/data_13_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

solve_aoc_13_part1(FileName, Out):-
    read_inputs_as_coords(FileName, Inputs),
    convlist(find_cost, Inputs, Costs),
    sumlist(Costs, Out).

read_inputs_as_coords(FileName, Inputs):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "", "\n\s\t", [MapStrings]),
    string_codes(MapStrings, Codes),
    phrase(s(Inputs), Codes).

s([Machine]) --> machine(Machine).
s([Machine|Machines]) --> machine(Machine),
                          eol,
                          eol,
                          s(Machines).


machine([[AX, AY], [BX, BY], [TX, TY]]) --> "Button A: X+",
                                            number(AX),
                                            ", Y+",
                                            number(AY),
                                            eol,
                                            "Button B: X+",
                                            number(BX),
                                            ", Y+",
                                            number(BY),
                                            eol,
                                            "Prize: X=",
                                            number(TX),
                                            ", Y=",
                                            number(TY).


% Also remember the 100 presses rule:
find_cost([[AX, AY], [BX, BY], [TargetX, TargetY]], Cost):-
    PressesA in 1..100,
    PressesB in 1..100,
    PressesA*AX + PressesB*BX #= TargetX,
    PressesA*AY + PressesB*BY #= TargetY,
    Cost #= 3*PressesA + PressesB.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:

aoc_13_test_part2(Out):-
    solve_aoc_13_part2("data/data_13_2024_test.txt", Out).

aoc_13_part2(Out):-
    solve_aoc_13_part2("data/data_13_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

% OK, general working on a scrap of paper now in
% the bin, but solve for A and B, where:
% 
% A*(X1)  = B*(X2) + (XT) 
%   (Y1)  =   (Y2) + (YT)
%
% Solution if A and B are integers.


solve_aoc_13_part2(FileName, Out):-
    read_inputs_as_coords(FileName, Inputs),
    convlist(find_cost2, Inputs, Costs),
    sumlist(Costs, Out).

find_cost2([ClawA, ClawB, [XT, YT]], Cost):-
    XT1 is XT + 10000000000000,
    YT1 is YT + 10000000000000,
    get_presses([ClawA, ClawB, [XT1, YT1]], [_, _, APresses, 0]),
    get_presses([ClawB, ClawA, [XT1, YT1]], [_, _, BPresses, 0]),
    Cost is APresses*3 + BPresses.

get_presses([[XA, YA], [XB, YB], [XT, YT]], [Dividend, Divisor, Quotient, Remainder]):-
    Dividend is (XT*YB - XB*YT),
    Divisor is (XA*YB - XB*YA),
    divmod(Dividend, Divisor, Quotient, Remainder).
