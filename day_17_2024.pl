%
% This year's compiler builder
%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library imports

:- use_module(library(lists)).
:- use_module(library(yall)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

% Actually easier just to type it in than bother parsing an input.
%
% It'll have to work at the command line :-).
%
% eg. 
% ?- run_program([0,1,5,4,3,0], computer(729,0,0,0,[]), Out). 
% Out = [4, 6, 3, 5, 6, 3, 5, 2, 1, 0] 

run_program(Program, A, B, C, Out):-
    run_program_(Program, computer(A, B, C, 0, []), Out).

run_program_(Program, computer(_A, _B, _C, IP, OutRev), Out):-
    length(Program, L),
    IP>=L,
    reverse(OutRev, Out).
run_program_(Program, Computer, Out):-
    apply(Program, Computer, Computer1),
    run_program_(Program, Computer1, Out).

% I'll represent the state as a structure:
%
% computer(A, B, C, IP, Output) where IP is the
% instruction pointer and Output is a list of outputs.

% combo operands
combo(0, _Computer, 0).
combo(1, _Computer, 1).
combo(2, _Computer, 2).
combo(3, _Computer, 3).
combo(4, computer(A, _B, _C, _IP, _Out), A).
combo(5, computer(_A, B, _C, _IP, _Out), B).
combo(6, computer(_A, _B, C, _IP, _Out), C).
combo(7, _Computer, error):-
    print("Error: 7 as combo operand").



% adv instruction
apply(Program, computer(A, B, C, IP, Out), computer(AOut, B, C, IPOut, Out)):-
    nth0(IP, Program, 0),
    succ(IP, OP),
    succ(OP, IPOut),
    nth0(OP, Program, Operand),

    combo(Operand, computer(A, B, C, IP, Out), OperandValue),
    AOut is A // (2**OperandValue).

% bxl instruction
apply(Program, computer(A, B, C, IP, Out), computer(A, BOut, C, IPOut, Out)):-
    nth0(IP, Program, 1),
    succ(IP, OP),
    succ(OP, IPOut),
    nth0(OP, Program, Operand),

    BOut is xor(B, Operand).

% bst instruction
apply(Program, computer(A, B, C, IP, Out), computer(A, BOut, C, IPOut, Out)):-
    nth0(IP, Program, 2),
    succ(IP, OP),
    succ(OP, IPOut),
    nth0(OP, Program, Operand),

    combo(Operand, computer(A, B, C, IP, Out), OperandValue),
    BOut is OperandValue mod 8.

% jnz instruction
apply(Program, computer(0, B, C, IP, Out), computer(0, B, C, IPOut, Out)):-
    nth0(IP, Program, 3),
    succ(IP, OP),
    succ(OP, IPOut).
apply(Program, computer(A, B, C, IP, Out), computer(A, B, C, Operand, Out)):-
    nth0(IP, Program, 3),
    succ(IP, OP),
    nth0(OP, Program, Operand),
    
    A \= 0.

% bxc instruction
apply(Program, computer(A, B, C, IP, Out), computer(A, BOut, C, IPOut, Out)):-
    nth0(IP, Program, 4),
    succ(IP, OP),
    succ(OP, IPOut),

    BOut is xor(B, C).

% out instruction
apply(Program, computer(A, B, C, IP, Out), computer(A, B, C, IPOut, [O|Out])):-
    nth0(IP, Program, 5),
    succ(IP, OP),
    succ(OP, IPOut),
    nth0(OP, Program, Operand),
    
    combo(Operand, computer(A, B, C, IP, Out), OperandValue),
    O is OperandValue mod 8.

% bdv instruction
apply(Program, computer(A, B, C, IP, Out), computer(A, BOut, C, IPOut, Out)):-
    nth0(IP, Program, 6),
    succ(IP, OP),
    succ(OP, IPOut),
    nth0(OP, Program, Operand),

    combo(Operand, computer(A, B, C, IP, Out), OperandValue),
    BOut is A // (2**OperandValue).

% cdv instruction
apply(Program, computer(A, B, C, IP, Out), computer(A, B, COut, IPOut, Out)):-
    nth0(IP, Program, 7),
    succ(IP, OP),
    succ(OP, IPOut),
    nth0(OP, Program, Operand),

    combo(Operand, computer(A, B, C, IP, Out), OperandValue),
    COut is A // (2**OperandValue).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:
%
% Lots of this was done at the command line.
%
% It turns out that when the value of register A is
% in octal, then the nth digit of A determines the 
% (L-n)th digit of the output, where L is the length
% of the octal representation of A.
%
% This is fiddly and (to me) unsatisfying, but there
% we go...
%
% Helpful to have a couple of predicates to convert
% between decimal and octal as a list. This comes for
% free in many other languages...

dec_to_oct_list(Dec, OctList):-
    MaxOctalPower1 is log(Dec)/log(8),
    floor(MaxOctalPower1, MaxOctalPower),
    dec_to_oct_list(Dec, MaxOctalPower, OctList).

dec_to_oct_list(Dec, 0, [Dec]).
dec_to_oct_list(Dec, Power, [Oct|Rest]):-
    Oct is Dec // (8**Power),
    Mod is Dec mod (8**Power),
    succ(P1, Power),
    dec_to_oct_list(Mod, P1, Rest).

oct_list_to_dec(ListIn, Out):-
    reverse(ListIn, RevList),
    oct_list_to_dec(RevList, 1, [], Nums),
    sumlist(Nums, Out).
oct_list_to_dec([], _, Out, Out).
oct_list_to_dec([Next|Rest], N, SoFar, Out):-
    N1 is N * 8,
    Next1 is Next * N,
    oct_list_to_dec(Rest, N1, [Next1|SoFar], Out).


aoc_17_test_part2(Computer):-
    aoc_17_quine([0,3,5,4,3,0], computer(0, 0, 0), Computer).

aoc_17_quine(Program, Computer, Computer):-
    run_program2(Program, Computer).

aoc_17_quine(Program, computer(A, B, C), Out):-
    succ(A, A1),
    aoc_17_quine(Program, computer(A1, B, C), Out).
    
aoc_17_part2(Out):-
    solve([2,4,1,4,7,5,4,1,1,4,5,5,0,3,3,0], ArgsOut),
    oct_list_to_dec(ArgsOut, Out).

solve(Program, ArgsOut):-
    length(Program, L),
    length(Args, L),
    maplist([X]>> =(X,0), Args),
    solve(Program, 0, L, Args, ArgsOut).

solve(_Program, L, L, ArgsOut, ArgsOut).
solve(Program, N, L, ArgsSoFar, ArgsOut):-
    N<L,
    set_n0th_arg(Program, N, ArgsSoFar, Args1),
    succ(N, N1),
    solve(Program, N1, L, Args1, ArgsOut).


set_n0th_arg(Program, N0, Args, UpdatedArgs):-
    between(0, 7, Arg),
    length(ArgsL, N0),
    append(ArgsL, [_|ArgsR], Args),
    append(ArgsL, [Arg|ArgsR], UpdatedArgs),
    oct_list_to_dec(UpdatedArgs, A),
    run_program(Program, A, 0, 0, ProgOut),
    reverse(Program, RevProgram),
    reverse(ProgOut, RevProgOut),
    nth0(N0, RevProgOut, V),
    nth0(N0, RevProgram, V).
