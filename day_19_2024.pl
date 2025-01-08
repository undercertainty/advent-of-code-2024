%
% Looks perfect Prolog fodder...
%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library imports

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AoC boilerplate

aoc_19_test_part1(Out):-
    solve_aoc_19_part1("data/data_19_2024_test.txt", Out).

aoc_19_part1(Out):-
    solve_aoc_19_part1("data/data_19_2024.txt", Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

solve_aoc_19_part1(FileName, Out):-
    get_input(FileName, (Towels, Designs)),!,
    include(valid_design(Towels), Designs, ValidDesigns),
    length(ValidDesigns, Out).

get_input(FileName, Out):-
    read_file_to_string(FileName, FileString, []),
    string_codes(FileString, Codes),
    phrase(day19(Out), Codes).


day19((Towels, Designs)) --> towels(Towels),
                             eol,
                             eol,
                             designs(Designs).

towels([Towel|Towels]) --> blanks,string(TowelCodes),
                                ",",
                                towels(Towels),
                                {atom_codes(TowelAtom, TowelCodes),
                                 atom_chars(TowelAtom, Towel)}.
towels([Towel]) --> blanks,string(TowelCodes),
                    {atom_codes(TowelAtom, TowelCodes),
                                 atom_chars(TowelAtom, Towel)}.

designs([Design]) --> nonblanks(DesignCodes),
                     {atom_codes(DesignAtom, DesignCodes),
                                 atom_chars(DesignAtom, Design)}.
designs([Design|Designs]) --> nonblanks(DesignCodes),
                              eol,
                              designs(Designs),
                               {atom_codes(DesignAtom, DesignCodes),
                                 atom_chars(DesignAtom, Design)}.

valid_design(_, []).
valid_design(Towels, Design):-
    member(Towel, Towels),
    append(Towel, DesignRest, Design),
    valid_design(Towels, DesignRest).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:
%
% I was most of the way there with this part,
% but have to confess that I needed a nudge
% from reddit to get the algorithm quite right.
% Annoyed with myself, 'cos this sort of dynamic
% programming task is classic traditional
% parsing stuff.

aoc_19_test_part2(Out):-
    solve_aoc_19_part2("data/data_19_2024_test.txt", Out).

aoc_19_part2(Out):-
    solve_aoc_19_part2("data/data_19_2024.txt", Out).

%%%%%%%%

solve_aoc_19_part2(FileName, Out):-
    get_input(FileName, (Towels, Designs)),!,
    maplist(count_arrangements(Towels), Designs, Arrangements),
    sumlist(Arrangements, Out).


%%% Well findall blows the stack, but thinking about it,
%%% it's just a chart parser...


count_arrangements(Towels, Design, Out):-
    get_edges(Design, Towels, Edges),
    !,
    length(Design, DesignLength),
    build_chart(Edges, DesignLength, Chart),
    last(Chart, Out).

% setof isn't very efficient, but it's not the
% bottleneck.

get_edges(Design, Towels, Edges):-
    setof(Edge, edge(Design, Towels, Edge), Edges).

edge(Design, Towels, edge(Start, End, Towel)):-
    member(Towel, Towels),
    append(DesignL, _DesignR, Design),
    append(LL, Towel, DesignL),
    length(LL, Start),
    length(Towel, TL),
    End is Start + TL.


build_chart(Edges, ChartLength, Chart):-
    length(EmptyChart, ChartLength),
    maplist(=(0), EmptyChart),
    build_chart(Edges, 1, [1|EmptyChart], Chart).

build_chart([], _, Chart, Chart).
build_chart([Edge|Edges], N, ChartSoFar, ChartOut):-
    select(edge(Start, N, Towel), [Edge|Edges], EdgesNext),
    add_edge(edge(Start, N, Towel), ChartSoFar, ChartNext),
    build_chart(EdgesNext, N, ChartNext, ChartOut).
build_chart([Edge|Edges], N, ChartSoFar, ChartOut):-
    \+ member(edge(_Start, N, _Towel), [Edge|Edges]),
    succ(N, N1),
    build_chart([Edge|Edges], N1, ChartSoFar, ChartOut).

add_edge(edge(Start, End, _Towel), ChartIn, ChartOut):-
    nth0(Start, ChartIn, StartCount),
    length(ChartLeft, End),
    append(ChartLeft, [EndCount|ChartRight], ChartIn),
    EndCount1 is StartCount + EndCount,
    append(ChartLeft, [EndCount1|ChartRight], ChartOut).