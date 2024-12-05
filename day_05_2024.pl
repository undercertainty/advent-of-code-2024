


%%% Day 05:

%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Library imports

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% AoC boilerplate

aoc_05_test_part1(Out):-
    solve_aoc_05_part1("data/data_05_2024_test.txt", Out).

aoc_05_part1(Out):-
    solve_aoc_05_part1("data/data_05_2024.txt", Out).

get_file_as_codes(FileName, Codes):-
    read_file_to_string(FileName, FileString, []),
    split_string(FileString, "", "\s\t\n", [StrippedFileString]),
    string_codes(StrippedFileString, Codes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Today's stuff

solve_aoc_05_part1(FileName, Out):-
    get_file_as_codes(FileName, Codes),
    phrase(aoc(OrderedPairs, Updates), Codes),
    
    findall(Central, (member(Update, Updates),
                      correctly_ordered(Update, OrderedPairs),
                      central_element(Update, Central)),
            CorrectUpdateCentrals),
    sumlist(CorrectUpdateCentrals, Out).


correctly_ordered(Update, OrderedPairs):-
    \+ 
        (member([N2, N1], OrderedPairs),
         append(_, [N1|Rest], Update),
         member(N2, Rest)).

% Bit inefficient, but this really isn't the bottleneck. Fails
% if even number of elements in list.
central_element(List, X):-
    append(Before, [X|After], List),
    length(Before, L),
    length(After, L).


aoc(Sec1, Sec2) --> sec1(Sec1),
                    eol,
                    sec2(Sec2).

sec1([]) --> [].
sec1([[N1, N2]|Rest]) --> integer(N1),
                          "|",
                          integer(N2),
                          eol,
                          sec1(Rest).


% sec1(Codes) --> string(Codes).
sec2([]) --> [].
sec2([Update|Rest]) --> update(Update),
                        eol,
                        sec2(Rest).

update([N]) --> integer(N).
update([N|R]) --> integer(N),
                  ",",
                  update(R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:

aoc_05_test_part2(Out):-
    solve_aoc_05_part2("data/data_05_2024_test.txt", Out).

aoc_05_part2(Out):-
    solve_aoc_05_part2("data/data_05_2024.txt", Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Part 2...

solve_aoc_05_part2(FileName, Out):-
    get_file_as_codes(FileName, Codes),
    phrase(aoc(OrderedPairs, Updates), Codes),

    findall(Central, (member(Update, Updates),
                      \+ correctly_ordered(Update, OrderedPairs),
                      reorder_update(Update, OrderedPairs, Reordered),
                      central_element(Reordered, Central)),
            CorrectedUpdateCentrals),
    sumlist(CorrectedUpdateCentrals, Out).

% Let's use an insertion sort-like sort to
% reorder the list:

reorder_update([], _, []).
reorder_update(Update, OrderedPairs, [Page|Rest]):-
    select(Page, Update, OtherPages),
    \+ (member(EarlierPage, OtherPages),
        member([EarlierPage, Page], OrderedPairs)),
    reorder_update(OtherPages, OrderedPairs, Rest).


