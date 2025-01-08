%
% Another Dijkstra.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library imports

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AoC map boilerplate

% Put in a threshold for minimum saving
aoc_20_test_part1(Out):-
    solve_aoc_20_part1("data/data_20_2024_test.txt", 10, Out).

aoc_20_part1(Out):-
    solve_aoc_20_part1("data/data_20_2024.txt", 100, Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

solve_aoc_20_part1(FileName, Threshold, Out):-
    get_input(FileName, maze(_Nodes, Path, _, _)),
    !,
    findall(Saving, (get_shortcut(Path, _, _, PathOut),
                     length(Path, OriginalLength),
                     length(PathOut, NewLength),
                     Saving is OriginalLength-NewLength,
                     Saving >= Threshold),
            Savings),
    length(Savings, Out).


% Get the graph representation as a chain of nodes.
% Then we should be able to chop bits out as needed
% and reevaluate the length.

get_input(FileName, maze(Nodes, PathEdges, (StartX, StartY), (EndX, EndY))):-
    read_file_to_string(FileName, FileString, []),
    grid_of_atoms(FileString, Maze),
    findall((X, Y), (nth0(Y, Maze, Row),
                     nth0(X, Row, Cell),
                     Cell \= '#'),
            Nodes),
    !,
    nth0(StartY, Maze, StartRow),
    nth0(StartX, StartRow, 'S'),
    nth0(EndY, Maze, EndRow),
    nth0(EndX, EndRow, 'E'),
    adjacent_node_edges(Nodes, Edges),
    find_path(Edges, (StartX, StartY), (EndX, EndY), PathEdges).

grid_of_atoms(InputString, Grid):-
    string_codes(InputString, Codes),
    phrase(grid(Grid), Codes).

grid([Row]) --> row(Row).
grid([Row|Rows]) --> row(Row),
                    eol,
                    grid(Rows).
row([]) --> [].
row([Atom|Row]) --> nonblank(Code),
                     row(Row),
                     {atom_codes(Atom, [Code])}.


% adjacent_node_edges/2
%
% takes a set of node coordinates in a grid, and returns
% edges.
adjacent_node_edges(Nodes, Edges):-
    findall(Node1-Node2, (member(Node1, Nodes),
                          member(Node2, Nodes),
                          adjacent_nodes(Node1, Node2)),
            Edges).

adjacent_nodes((X1, Y1), (X2, Y1)):-
    succ(X1, X2).
adjacent_nodes((X1, Y1), (X2, Y1)):-
    succ(X2, X1).
adjacent_nodes((X1, Y1), (X1, Y2)):-
    succ(Y1, Y2).
adjacent_nodes((X1, Y1), (X1, Y2)):-
    succ(Y2, Y1).

% There's only one path, so it should be easy enough
% to follow. We can sort the list of edges so that
% they form a chain.

find_path(Edges, Start, End, Path):-
    find_path_(Edges, Start, [End], Path).

find_path_(Edges, Start, [Loc|Path], [Start, Loc|Path]):-
    member(Start-Loc, Edges).
find_path_(Edges, Start, [Loc|Path], Out):-
    select(NextLoc-Loc, Edges, NextEdges),
    \+ member(NextLoc, Path),
    !,
    find_path_(NextEdges, Start, [NextLoc, Loc|Path], Out).


get_shortcut(PathIn, (StartX, Y), (EndX, Y), PathOut):-
    append(PathStart, [(StartX, Y)|PathRest], PathIn),
    succ(StartX, MidX),
    succ(MidX, EndX),
    \+ member((MidX, Y), PathIn),
    append(_, [(EndX, Y)|PathEnd], PathRest),
    append(PathStart, [(StartX, Y), (MidX, Y), (EndX, Y)|PathEnd], PathOut).

get_shortcut(PathIn, (StartX, Y), (EndX, Y), PathOut):-
    append(PathStart, [(StartX, Y)|PathRest], PathIn),
    succ(MidX, StartX),
    succ(EndX, MidX),
    \+ member((MidX, Y), PathIn),
    append(_, [(EndX, Y)|PathEnd], PathRest),
    append(PathStart, [(StartX, Y), (MidX, Y), (EndX, Y)|PathEnd], PathOut).

get_shortcut(PathIn, (X, StartY), (X, EndY), PathOut):-
    append(PathStart, [(X, StartY)|PathRest], PathIn),
    succ(StartY, MidY),
    succ(MidY, EndY),
    \+ member((X, MidY), PathIn),
    append(_, [(X, EndY)|PathEnd], PathRest),
    append(PathStart, [(X, StartY), (X, MidY), (X, EndY)|PathEnd], PathOut).

get_shortcut(PathIn, (X, StartY), (X, EndY), PathOut):-
    append(PathStart, [(X, StartY)|PathRest], PathIn),
    succ(MidY, StartY),
    succ(EndY, MidY),
    \+ member((X, MidY), PathIn),
    append(_, [(X, EndY)|PathEnd], PathRest),
    append(PathStart, [(X, StartY), (X, MidY), (X, EndY)|PathEnd], PathOut).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:
%
% A generalisation of part 1.

aoc_20_test_part2(Out):-
    solve_aoc_20_part2("data/data_20_2024_test.txt", 20, 70, Out).

aoc_20_part2(Out):-
    solve_aoc_20_part2("data/data_20_2024.txt", 20, 100, Out).


% Could have used this for the first part:

%aoc_20_test_part1(Out):-
%    solve_aoc_20_part2("data/data_20_2024_test.txt", 2, 10, Out).
%
%aoc_20_part1(Out):-
%    solve_aoc_20_part2("data/data_20_2024.txt", 2, 100, Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

solve_aoc_20_part2(FileName, MaxCheatLength, Threshold, Out):-
    get_input(FileName, maze(_Nodes, Path, _, _)),
    !,
    findall(Saving, (get_shortcut_2(Path, MaxCheatLength, _Start, _End, Saving),
                     Saving>=Threshold), 
            Savings),
    length(Savings, Out).

% I'll do more of the calculation in the actual 
% predicate here.
%
% Don't actually need to return Start and End ,
% but might be useful for debugging
%
% I'm not going to bother working out the actual
% shortcut, so no point in returning an output
% path.

get_shortcut_2(Path, MaxCheatLength, (X1,Y1), (X2, Y2), Saving):-
    append(_PathStart, [(X1, Y1)|Path1], Path),
    append(ShortCut, [(X2, Y2)|_PathEnd], Path1),
    length([(X1, Y1)|ShortCut], ShortCutLength),
    manhattan_distance((X1, Y1), (X2, Y2), CheatLength),
    CheatLength =< MaxCheatLength,
    Saving is ShortCutLength-CheatLength.

manhattan_distance((X1, Y1), (X2, Y2), Out):-
    Out is abs(X1-X2) + abs(Y1-Y2).