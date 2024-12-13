


%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library imports

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(readutil)).
:- use_module(library(yall)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AoC map boilerplate

aoc_12_test_part1(Out):-
    solve_aoc_12_part1("data/data_12_2024_test.txt", Out).

aoc_12_part1(Out):-
    solve_aoc_12_part1("data/data_12_2024.txt", Out).


get_file_as_map(FileName, map([Row|Rows], Width, Height)):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "\n", "\n\s\t", MapStrings),
    maplist([X, Y]>>(string_lower(X, Z), 
                     string_codes(Z, Cs), 
                     maplist([A, B]>>atom_char(B, A), Cs, Y)),
            MapStrings,
            [Row|Rows]),
    length([Row|Rows], Height),
    length(Row, Width).

%%% Fail if loc not in the map

get_map_value(X, Y, map(Map, _, _), Val):-
    nth1(Y, Map, Row),
    nth1(X, Row, Val).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff


solve_aoc_12_part1(FileName, Out):-
    get_file_as_map(FileName, Map),
    findall(Plot, (get_map_value(X, Y, Map, _V), 
                   get_plot(X, Y, Map, Plot)),
            Plots),
    list_to_set(Plots, PlotSet),
    maplist(plot_cost, PlotSet, PlotCosts),
    sumlist(PlotCosts, Out).


% Use a flood fill to find each plot

:- dynamic(filled/3).
:- dynamic(edge/3).


get_plot(X, Y, Map, [SortedSpaces, SortedEdges]):-
    retractall(filled(_, _, _)),
    retractall(edge(_, _, _)),
    fill(X, Y, Map),
    findall([A, B, C], filled(A, B, C), Spaces),
    findall([A, B, C], edge(A, B, C), Edges),
    % Sort to make comparison easier
    sort(Spaces, SortedSpaces),
    sort(Edges, SortedEdges).
    

fill(X, Y, _Map):-
    filled(X, Y, _).

fill(X, Y, Map):-
    \+ filled(X, Y, _),
    get_map_value(X, Y, Map, Value),
    assert(filled(X, Y, Value)),
    succ(XLeft, X),
    (
        get_map_value(XLeft, Y, Map, Value)
        ->
        fill(XLeft, Y, Map)
        ;
        assert(edge(X, Y, '<'))
    ),
    succ(X, XRight),
    (
        get_map_value(XRight, Y, Map, Value)
        ->
        fill(XRight, Y, Map)
        ;
        assert(edge(X, Y, '>'))
    ),
    succ(YUp, Y),
    (
        get_map_value(X, YUp, Map, Value)
        ->
        fill(X, YUp, Map)
        ;
        assert(edge(X, Y, '^'))
    ),
    succ(Y, YDown),
    (
        get_map_value(X, YDown, Map, Value)
        ->
        fill(X, YDown, Map)
        ;
        assert(edge(X, Y, 'v'))
    ).

plot_cost([Spaces, Edges], Out):-
    length(Spaces, NumSpaces),
    length(Edges, NumEdges),
    Out is NumSpaces * NumEdges.
    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:

aoc_12_test_part2(Out):-
    solve_aoc_12_part2("data/data_12_2024_test.txt", Out).

aoc_12_part2(Out):-
    solve_aoc_12_part2("data/data_12_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Part 2's easy now: just part 1 without the filter:

solve_aoc_12_part2(FileName, Out):-
    get_file_as_map(FileName, Map),
    findall(Plot, (get_map_value(X, Y, Map, _V), 
                   get_plot2(X, Y, Map, Plot)),
            Plots),
    list_to_set(Plots, PlotSet),
    maplist(plot_cost, PlotSet, PlotCosts),
    sumlist(PlotCosts, Out).



% Should be able to reduce the edges quite simply...
%
% Slight change to get_plot to return a range rather 
% than a value:

get_plot2(X, Y, Map, [SortedSpaces, SortedEdges]):-
    retractall(filled(_, _, _)),
    retractall(edge(_, _, _)),
    fill(X, Y, Map),
    findall([A, B, C], filled(A, B, C), Spaces),
    findall([A-A, B-B, C], edge(A, B, C), Edges),
    % Sort to make comparison easier
    reduce_edges(Edges, ReducedEdges),
    sort(Spaces, SortedSpaces),
    sort(ReducedEdges, SortedEdges),!.


reduce_edges(Edges, Out):-
    select([X1-X2, Y, '^'], Edges, Edges1),
    select([X3-X4, Y, '^'], Edges1, Edges2),
    succ(X2, X3),
    reduce_edges([[X1-X4, Y, '^']|Edges2], Out).

reduce_edges(Edges, Out):-
    select([X1-X2, Y, v], Edges, Edges1),
    select([X3-X4, Y, v], Edges1, Edges2),
    succ(X2, X3),
    reduce_edges([[X1-X4, Y, v]|Edges2], Out).

reduce_edges(Edges, Out):-
    select([X, Y1-Y2, '<'], Edges, Edges1),
    select([X, Y3-Y4, '<'], Edges1, Edges2),
    succ(Y2, Y3),
    reduce_edges([[X, Y1-Y4, '<']|Edges2], Out).

reduce_edges(Edges, Out):-
    select([X, Y1-Y2, '>'], Edges, Edges1),
    select([X, Y3-Y4, '>'], Edges1, Edges2),
    succ(Y2, Y3),
    reduce_edges([[X, Y1-Y4, '>']|Edges2], Out).

reduce_edges(Edges, Edges):-!.

