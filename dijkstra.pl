
% Worth having a standard implementation of this, as
% it seems to come up every year.
%
% Assume that there are facts of the form node/1 and edge/3,
% where the three arguments to edge are FromNode, ToNode
% and Weight.
%
% Damn it, looks as though SWI doesn't have updating the heap
% functionality...

:- use_module(library(lists)).
:- use_module(library(heaps)).
:- use_module(library(rbtrees)).


dijkstra_test([Dists, Prevs]):-
    shortest_paths([a, b, c, d, e, f],
                  [[a, b, 3], [b, a, 3], [a, d, 8], [d, a, 8], [b, d, 5],
                   [d, b, 5], [b, e, 6], [e, b, 6], [d, f, 2], [f, d, 2],
                   [e, c, 9], [c, e, 9], [c, f, 3], [f, c, 3], [e, f, 1], 
                   [f, e, 1], [e, d, 3], [d, e, 3]],
                   a,
                   Dists,
                   Prevs).

% Haven't implemented target yet... maybe later
dijkstra_test(Target, X):-
    shortest_paths([a, b, c, d, e, f],
                  [[a, b, 3], [b, a, 3], [a, d, 8], [d, a, 8], [b, d, 5],
                   [d, b, 5], [b, e, 6], [e, b, 6], [d, f, 2], [f, d, 2],
                   [e, c, 9], [c, e, 9], [c, f, 3], [f, c, 3], [e, f, 1], 
                   [f, e, 1], [e, d, 3], [d, e, 3]],
                   a,
                   Target,
                   X).

% Nodes = list of nodes, [Node1, Node2, Node3|...]
%
% Edges = list of triples: [[NodeFrom, NodeTo, Weight]|...]
%
% StartNode in Nodes.

t(Nodes, Edges):-
    Nodes=[a, b, c, d, e, f],
    Edges=[[a, b, 3], [b, a, 3], [a, d, 8], [d, a, 8], [b, d, 5],
                   [d, b, 5], [b, e, 6], [e, b, 6], [d, f, 2], [f, d, 2],
                   [e, c, 9], [c, e, 9], [c, f, 3], [f, c, 3], [e, f, 1], 
                   [f, e, 1], [e, d, 3], [d, e, 3]].


shortest_paths(Nodes, Edges, StartNode, DistsOut, PrevsOut):-

    % Create queue
    select(StartNode, Nodes, Nodes1),
    findall(inf-Node, member(Node, Nodes1), ListHeap),
    list_to_heap([0-StartNode|ListHeap], Queue),

    % rb_tree of distances
    findall(Node-inf, member(Node, Nodes1), ListRbTreeDist),
    list_to_rbtree([StartNode-0|ListRbTreeDist], Dists),

    % rb_tree of previous nodes
    findall(Node-[], member(Node, Nodes), ListRbTreePrev),
    list_to_rbtree(ListRbTreePrev, Prevs),

    % Do the search
    dijkstra_search(Queue, Edges, Dists, Prevs, RBDists, RBPrevs),
    findall(N-D, rb_in(N, D, RBDists), DistsOut),
    findall(N-P, rb_in(N, P, RBPrevs), PrevsOut).


dijkstra_search(Queue, Edges, Dists, Prevs, DistsOut, PrevsOut):-
    dijkstra_search(Queue, Edges, Dists, Prevs, [], DistsOut, PrevsOut).

dijkstra_search(Queue, _Edges, Dists, Prevs, Visited, Dists, Prevs):-
    \+ get_from_queue(Queue, _DistU, _U, Visited, _Queue1).
dijkstra_search(Queue, Edges, Dists, Prevs, Visited, DistsOut, PrevsOut):-
    get_from_queue(Queue, DistU, U, Visited, Queue1),
    findall([U, V, Weight],
            (member([U, V, Weight], Edges), \+ member(V, Visited)),
            UVEdges),
    update_queue(Queue1, Dists, Prevs, DistU, UVEdges, QueueNext, DistsNext, PrevsNext),
    dijkstra_search(QueueNext, Edges, DistsNext, PrevsNext, [U|Visited], DistsOut, PrevsOut).

% Draw items from the queue until we get one that's not
% been visited (as there's no update_heap predicate)
get_from_queue(Queue, DistU, U, Visited, QueueOut):-
    get_from_heap(Queue, DistU, U, QueueOut),
    \+ member(U, Visited).
get_from_queue(Queue, DistU, U, Visited, QueueOut):-
    get_from_heap(Queue, _DistU1, U1, Queue1),
    member(U1, Visited),
    get_from_queue(Queue1, DistU, U, Visited, QueueOut).

% If shorter paths, add to the current queue
update_queue(Queue, Dists, Prevs, _DistU, [], Queue, Dists, Prevs).
update_queue(Queue, Dists, Prevs, DistU, [[U, V, Weight]|UVEdges], QueueOut, DistsOut, PrevsOut):-
    rb_lookup(V, DistV, Dists),
    Alt is DistU + Weight,
    (
        Alt =< DistV
    ->
        add_to_heap(Queue, Alt, V, QueueNext),
        rb_update(Dists, V, Alt, DistsNext),
        rb_update(Prevs, V, P, [U|P], PrevsNext)
    ;
        QueueNext=Queue,
        DistsNext=Dists,
        PrevsNext=Prevs
    ),
    update_queue(QueueNext, DistsNext, PrevsNext, DistU, UVEdges, QueueOut,  DistsOut, PrevsOut).







