%
% This year's (first?) outing for Dijkstra...
%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library imports

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(yall)).
:- use_module(library(dcg/basics)).

:- [dijkstra].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AoC map boilerplate

aoc_16_test_part1(Out):-
    solve_aoc_16_part1("data/data_16_2024_test.txt", Out).

aoc_16_part1(Out):-
    solve_aoc_16_part1("data/data_16_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

solve_aoc_16_part1(FileName, Out):-
    get_input(FileName, Input),!,
    build_map(Input, map(Nodes, Edges)),
    get_start(Input, InitialNode),
    get_end(Input, FinalNode),
    shortest_paths(Nodes, Edges, InitialNode, ShortestPaths, _PreviousNodes),
    findall(CostToE, member(FinalNode-CostToE, ShortestPaths), CostsToE),
    min_member(Out, CostsToE).

get_input(FileName, Input):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "", "\n\s\t", [MapStrings]),
    string_codes(MapStrings, Codes),
    grid_of_atoms(Codes, Input).

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

% Build nodes/edges rep from list of lists
build_map(Map, map(Nodes, Edges)):-
    findall(Node, node(Node, Map), Nodes),
    findall(Edge, edge(Edge, Nodes), Edges).

node([X, Y, Facing], Map):-
    nth0(Y, Map, Row),
    nth0(X, Row, Tile),
    member(Tile, ['.', 'S', 'E']),
    member(Facing, ['^', 'v', '<', '>']).

edge([[X, Y, '>'], [X1, Y, '>'], 1], Nodes):-
    member([X, Y, '>'], Nodes),
    succ(X, X1),
    member([X1, Y, '>'], Nodes).
edge([[X, Y, '<'], [X1, Y, '<'], 1], Nodes):-
    member([X, Y, '<'], Nodes),
    succ(X1, X),
    member([X1, Y, '<'], Nodes).
edge([[X, Y, '^'], [X, Y1, '^'], 1], Nodes):-
    member([X, Y, '^'], Nodes),
    succ(Y1, Y),
    member([X, Y1, '^'], Nodes).
edge([[X, Y, 'v'], [X, Y1, 'v'], 1], Nodes):-
    member([X, Y, 'v'], Nodes),
    succ(Y, Y1),
    member([X, Y1, 'v'], Nodes).

edge([[X, Y, '^'], [X, Y, '>'], 1000], Nodes):-
    member([X, Y, '^'], Nodes).
edge([[X, Y, '^'], [X, Y, '<'], 1000], Nodes):-
    member([X, Y, '^'], Nodes).
edge([[X, Y, 'v'], [X, Y, '>'], 1000], Nodes):-
    member([X, Y, 'v'], Nodes).
edge([[X, Y, 'v'], [X, Y, '<'], 1000], Nodes):-
    member([X, Y, 'v'], Nodes).
edge([[X, Y, '<'], [X, Y, '^'], 1000], Nodes):-
    member([X, Y, '<'], Nodes).
edge([[X, Y, '<'], [X, Y, 'v'], 1000], Nodes):-
    member([X, Y, '<'], Nodes).
edge([[X, Y, '>'], [X, Y, '^'], 1000], Nodes):-
    member([X, Y, '>'], Nodes).
edge([[X, Y, '>'], [X, Y, 'v'], 1000], Nodes):-
    member([X, Y, '>'], Nodes).

get_start(Map, [X, Y, '>']):-
    nth0(Y, Map, Row),
    nth0(X, Row, 'S').

get_end(Map, [X, Y, _]):-
    nth0(Y, Map, Row),
    nth0(X, Row, 'E').




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:
%
% Need the paths for this...

aoc_16_test1_part2(Out):-
    solve_aoc_16_part2("data/data_16_2024_test.txt", Out).

aoc_16_test2_part2(Out):-
    solve_aoc_16_part2("data/data_16_2024_test_2.txt", Out).

aoc_16_part2(Out):-
    solve_aoc_16_part2("data/data_16_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

solve_aoc_16_part2(FileName, Out):-
    get_input(FileName, Input),!,
    build_map(Input, map(Nodes, Edges)),
    get_start(Input, InitialNode),
    get_end(Input, FinalNode),
    shortest_paths(Nodes, Edges, InitialNode, ShortestPaths, PreviousNodes),
    findall(CostToE-FinalNode, member(FinalNode-CostToE, ShortestPaths), CostsToE),
    min_member(_Cost-ShortestFinalNode, CostsToE),
    findall(Path, get_path(ShortestFinalNode, PreviousNodes, Path), Paths),
    get_best_path_tiles(Paths, BestPathTiles),
    list_to_set(BestPathTiles, UniqueTiles),
    length(UniqueTiles, Out).


get_path(Node, PreviousNodes, [Node]):-
    member(Node-[], PreviousNodes).
get_path(Node, PreviousNodes, [Node|Path]):-
    member(Node-Priors, PreviousNodes),
    member(NodeU, Priors),
    get_path(NodeU, PreviousNodes, Path).

get_best_path_tiles([], []).
get_best_path_tiles([Path|Paths], Out):-
    get_best_path_tiles(Paths, FlatPaths),
    maplist([X, Y]>>(append(Y, _, X), Y=[_, _]), Path, FlatPath),
    append(FlatPath, FlatPaths, Out).

