% Custom predicate to find the maximum element in a list
max_list([H|T], Max) :- max_list(T, H, Max).

max_list([], Max, Max).
max_list([H|T], Max, Result) :-
    H >= Max,
    max_list(T, H, Result).
max_list([H|T], Max, Result) :-
    H < Max,
    max_list(T, Max, Result).

% Predicate to find the size of the largest cluster for a given type
largest_cluster_size(Board, Type, Size) :-
    findall(ClusterSize, (
        valid_position(Board, Type, X, Y), 
        %\+ memberchk([X, Y], Visited), 
        largest_cluster(Board, Type, X, Y, [], 0, ClusterSize)
    ), Sizes),
    max_list(Sizes, 0, Size).

% Predicate to perform DFS to find the size of a cluster
largest_cluster(_, _, _, _, _, CurrentSize, CurrentSize).

% largest_cluster(Board, Type, X, Y, Visited, CurrentSize, ClusterSize) :-
%     valid_position(Board, Type, X, Y),
%     \+ memberchk([X, Y], Visited),
%     get_adjacent_indexes(X, Y, AdjacentPositions),
%     NewSize is CurrentSize + 1,
%     nl, nl,
%     write('Position: '), write([X,Y]), nl,
%     write('New size: '), write(NewSize), nl,
%     append([[X, Y]], Visited, NewVisited),
%     findall(Size, (
%         member([NewX, NewY], AdjacentPositions),
%         \+ memberchk([NewX, NewY], Visited),
%         largest_cluster(Board, Type, NewX, NewY, NewVisited, NewSize, Size)
%     ), Sizes),
%     write('Sizes: '), write(Sizes), nl,
%     append(Sizes, [NewSize], ClusterSizes),
%     max_list(ClusterSizes, ClusterSize).

largest_cluster(Board, Type, X, Y, Visited, CurrentSize, ClusterSize) :-
    
    get_adjacent_indexes(X, Y, AdjacentPositions),
    NewSize is 1,
    % nl, nl,
    % write('Position: '), write([X,Y]), nl,
    % write('New size: '), write(NewSize), nl,
    append([[X, Y]], Visited, NewVisited),
    findall(Size, (
        member([NewX, NewY], AdjacentPositions),
        valid_position(Board, Type, NewX, NewY),
        \+ memberchk([NewX, NewY], NewVisited),
        largest_cluster(Board, Type, NewX, NewY, NewVisited, NewSize, Size)
    ), Sizes), 
    % write('Sizes: '), write(Sizes), nl,
    length(Sizes, N),
    (N == 0 ->
        ClusterSize is 1;
        ( N == 2 ->
            sum_list(Sizes, ClusterSize);  
            Pad is integer((N/2) - 1),
            sum_list(Sizes, Temp),
            ClusterSize is Temp - Pad
        )
    ).


% Predicate to get the adjacent positions orthogonally (up, down, left, right)
get_adjacent_indexes(X, Y, AdjacentPositions) :-
    Y1 is Y-1, Y2 is Y+1, X1 is X-1, X2 is X+1,
    AdjacentPositions = [[X, Y1], [X, Y2], [X1, Y], [X2, Y]].
  
valid_position(Board, Type, X, Y) :-
    nth0(Y, Board, Row),
    nth0(X, Row, Cell),
    Cell = Type.


board4x4([
    ['X', 'X', 'X', 'X'],
    ['X', 'O', 'X', 'O'],
    ['O', 'O', 'X', 'X'],
    ['X', 'O', 'O', 'X']
]).

% X - 6
% O - 7
board8x8([
    ['X', 'X', 'X', 'X', 'X', 'O', 'O', 'O'],
    ['O', 'X', 'O', 'O', 'O', 'X', 'X', 'O'],
    ['X', 'O', 'X', 'O', 'X', 'O', 'X', 'O'],
    ['O', 'X', 'O', 'X', 'O', 'X', 'O', 'X'],
    ['X', 'O', 'X', 'O', 'X', 'O', 'X', 'O'],
    ['X', 'O', 'X', 'X', 'O', 'O', 'O', 'O'],
    ['X', 'X', 'O', 'X', 'O', 'O', 'O', 'X'],
    ['X', 'O', 'X', 'O', 'X', 'X', 'X', 'O']
]).

test :-
    board8x8(Board),
    largest_cluster_size(Board, 'X', LargestXClusterSize),
    write('Largest X cluster size: '), write(LargestXClusterSize), nl,
    largest_cluster_size(Board, 'O', LargestOClusterSize),
    write('Largest O cluster size: '), write(LargestOClusterSize), nl.

test1(L) :-
    board4x4(Board),
    valid_position(Board, 'X', X, Y),
    L = [X, Y].

test2 :-
    board4x4(Board),
    X is 0, Y is 0,
    CurrentSize is 0,
    Type = 'X',
    Visited = [],

    board_get_element(Board, [X,Y], Cell),
    write('Cell: '), write(Cell), nl,
    valid_position(Board, Type, X, Y),
    get_adjacent_indexes(X, Y, AdjacentPositions),
    write('Adjacent positions: '), write(AdjacentPositions), nl,
    NewSize is 1,
    write('New size: '), write(NewSize), nl,
    append([[X, Y]], Visited, NewVisited),
    write('New visited: '), write(NewVisited), nl,
    findall(Size, (
        member([NewX, NewY], AdjacentPositions),
        valid_position(Board, Type, NewX, NewY),
        \+ memberchk([NewX, NewY], NewVisited),
        largest_cluster(Board, Type, NewX, NewY, NewVisited, NewSize, Size)
    ), Sizes),
    write('Sizes: '), write(Sizes), nl,
    length(Sizes, N),
    (N == 0 ->
        ClusterSize is 1;
        ( N == 2 ->
            sum_list(Sizes, ClusterSize);  
            Pad is integer((N/2) - 1),
            sum_list(Sizes, Temp),
            ClusterSize is Temp - Pad
        )
    ),
    write('Max cluster size: '), write(ClusterSize), nl.

    % nl,nl,
    % largest_cluster(Board, Type, X, Y, [], CurrentSize, ClusterSize),
    % write('Cluster size: '), write(ClusterSize), nl.


test3 :-
    board4x4(Board),
    X is 0, Y is 0,
    CurrentSize is 0,
    Type = 'X',
    Visited = [],
    largest_cluster(Board, Type, X, Y, Visited, CurrentSize, ClusterSize),
    write('Max cluster size: '), write(ClusterSize), nl.
