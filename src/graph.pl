:- dynamic visited/2.

% Define directions for the flood-fill algorithm
directions([0-1, 1-0, 0-(-1), -1-0]).

% Flood-fill from a given position
flood_fill(Board, X, Y, Piece, Size) :-
    board_size(Board, N),                   % Get board size
    valid_position(N, X, Y),
    \+ visited(X, Y),                   % Check if not visited
    board_get_element(Board, [X, Y], Piece),
    asserta(visited(X, Y)),             % Mark as visited
    directions(Dirs),                   
    flood_fill_neighbours(Board, X, Y, Dirs, Piece, Sizes), % Flood-fill neighbours
    sum_list(Sizes, NeighboursSize),                        
    Size is 1 + NeighboursSize.                              % Size including this piece

flood_fill(_, _, _, _, 0).

% Flood-fill neighbouring cells
flood_fill_neighbours(Board, X, Y, [DX-DY|RestDirs], Piece, [NeighbourSize|Sizes]) :-
    NewX is X + DX, NewY is Y + DY,
    flood_fill(Board, NewX, NewY, Piece, NeighbourSize), % Recursive call
    flood_fill_neighbours(Board, X, Y, RestDirs, Piece, Sizes).
flood_fill_neighbours(_, _, _, [], _, []).

% Find the largest group on the board for a given piece
find_largest_group(Board, Piece, LargestSize) :-
    board_size(Board, N), N1 is N - 1,                             % Get board size
    findall(Size, (between(0, N1, X), between(0, N1, Y), 
                   flood_fill(Board, X, Y, Piece, Size)), Sizes), % Find all sizes
    max_list(Sizes, LargestSize).                                   % Find the max size

% Main predicate to find the largest group for 'X' and 'O'
largest_groups(Board, LargestX, LargestO) :-
    retractall(visited(_,_)), % Clear visited cells
    find_largest_group(Board, 'X', LargestX),
    retractall(visited(_,_)), % Clear visited cells again for 'O'
    find_largest_group(Board, 'O', LargestO).

% Test predicate
test :-
    Board = [['X', 'X', 'X', 'X', 'O'],
             ['X', 'X', 'O', 'O', 'O'],
             ['X', 'O', 'X', 'X', 'O'],
             ['X', 'O', 'O', 'X', 'O'],
             ['X', 'O', 'X', 'O', 'O']],
    largest_groups(Board, LargestX, LargestO),
    format("Largest X group: ~d~nLargest O group: ~d", [LargestX, LargestO]).
