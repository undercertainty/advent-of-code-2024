%
% Another Dijkstra.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library imports

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).

:- [dijkstra].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AoC map boilerplate

aoc_18_test_part1(Out):-
    solve_aoc_18_part1("data/data_18_2024_test.txt", 6, 12, Out).

aoc_18_part1(Out):-
    solve_aoc_18_part1("data/data_18_2024.txt", 70, 1024, Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff


solve_aoc_18_part1(FileName, Dimension, Limit, Out):-
    get_input(FileName, Coords),!,
    length(CorruptedLocations, Limit),
    append(CorruptedLocations, _, Coords),
    findall((X, Y), (between(0, Dimension, X),
                     between(0, Dimension, Y),
                     \+ member((X, Y), CorruptedLocations)),
            SafeLocations),
    adjacent_node_edges(SafeLocations, AdjacentNodeEdges),
    shortest_paths(SafeLocations, AdjacentNodeEdges, (0, 0), Dists, _Prevs),
    member((Dimension, Dimension)-Out, Dists).

t(Out):-
    get_input("data/data_18_2024_test.txt", Out).


get_input(FileName, Coords):-
    read_file_to_string(FileName, FileString, []),
    string_codes(FileString, Codes),
    phrase(coords(Coords), Codes).

coords([]) --> blanks.

coords([(X, Y)|Coords]) --> number(X),
                            ",",
                            number(Y),
                            eol,
                            coords(Coords).



% adjacent_node_edges/2
%
% takes a set of node coordinates in a grid, and returns
% edges so that each pair of adjacent nodes is separated
% by a distance of 1.
adjacent_node_edges(Nodes, Edges):-
    findall([Node1, Node2, 1], (member(Node1, Nodes),
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:
%
% Straightforward enough, and less interesting than I
% was expecting. Just going to blast it...

aoc_18_test_part2(Out):-
    solve_aoc_18_part2("data/data_18_2024_test.txt", 6, Out).

aoc_18_part2(Out):-
    solve_aoc_18_part2("data/data_18_2024.txt", 70, Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

% Should be able to just find the first case that fails.

solve_aoc_18_part2(FileName, Dimension, Out):-
    get_input(FileName, Coords),!,
    append(CorruptedLocations, _, Coords),
    findall((X, Y), (between(0, Dimension, X),
                     between(0, Dimension, Y),
                     \+ member((X, Y), CorruptedLocations)),
            SafeLocations),
    adjacent_node_edges(SafeLocations, AdjacentNodeEdges),
    shortest_paths(SafeLocations, AdjacentNodeEdges, (0, 0), Dists, _Prevs),
    member((Dimension, Dimension)-inf, Dists),
    last(CorruptedLocations, Out).

