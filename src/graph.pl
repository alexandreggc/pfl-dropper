:- dynamic visited/2.

/* Define directions for the flood-fill algorithm */
directions([0-1, 1-0, 0-(-1), -1-0]).


/* Flood-fill from a given position */
%flood_fill(+Board, +X, +Y, +Piece, -Size)
flood_fill(Board, X, Y, Piece, Size) :-
    board_size(Board, N),
    valid_position(N, X, Y),
    \+ visited(X, Y),
    board_get_element(Board, [X, Y], Piece),
    asserta(visited(X, Y)),
    directions(Dirs),                   
    flood_fill_neighbours(Board, X, Y, Dirs, Piece, Sizes),
    sum_list(Sizes, NeighboursSize),
    Size is 1 + NeighboursSize.

flood_fill(_, _, _, _, 0).

/* Flood-fill neighbouring cells */
%flood_fill_neighbours(+Board, +X, +Y, +Dirs, +Piece, -Sizes)
flood_fill_neighbours(Board, X, Y, [DX-DY|RestDirs], Piece, [NeighbourSize|Sizes]) :-
    NewX is X + DX, NewY is Y + DY,
    flood_fill(Board, NewX, NewY, Piece, NeighbourSize),
    flood_fill_neighbours(Board, X, Y, RestDirs, Piece, Sizes).

flood_fill_neighbours(_, _, _, [], _, []).

/* Find the ordered list of groups on the board for a given piece */
%find_ordered_groups(+Board, +Piece, -Sizes)
find_ordered_groups(Board, Piece, Sizes) :-
    board_size(Board, N), N1 is N - 1,
    findall(Size, (between(0, N1, X), between(0, N1, Y),
                   flood_fill(Board, X, Y, Piece, Size)), TempSizes),
    sort(TempSizes, OrderedSizes),
    reverse(OrderedSizes, Sizes).



/* Main predicate to find the largest diferent groups for 'X' and 'O' */
%best_groups(+Board, -LargestX, -LargestO)
best_groups(Board, LargestX, LargestO) :-
    retractall(visited(_,_)), % Clear visited cells
    find_ordered_groups(Board, 'X', SizesX),
    retractall(visited(_,_)), % Clear visited cells again for 'O'
    find_ordered_groups(Board, 'O', SizesO),
    find_group_combination(SizesX, SizesO, LargestX, LargestO).

/* Find largest group for each of the playes */
%largest_groups(+Board, +largestX, -largestO)
largest_groups(Board, LargestX, LargestO) :-
    retractall(visited(_,_)), % Clear visited cells
    find_ordered_groups(Board, 'X', SizesX),
    retractall(visited(_,_)), % Clear visited cells again for 'O'
    find_ordered_groups(Board, 'O', SizesO),
    max_list(SizesX, LargestX),
    max_list(SizesO, LargestO).


/* Find best group combination taking into to consideration the draw cases*/
%find_group_combination(+Board, +Piece, -Sizes)
find_group_combination([SizeX | SizesX], [SizeO | SizesO], LargestX, LargestO) :-
    ((SizesX == [0] ; SizesO == [0]) ->
        LargestX is SizeX,
        LargestO is SizeO;
        (SizeO =\= SizeX ->
            LargestX is SizeX,
            LargestO is SizeO;
            find_group_combination(SizesX, SizesO, LargestX, LargestO)
        )
    ).


/* Test predicate */
test :-
    Board = [['X', 'X', 'X', 'X', 'O'],
             ['X', 'X', 'O', 'O', 'O'],
             ['X', 'O', 'X', 'X', 'O'],
             ['X', 'O', 'O', 'X', 'O'],
             ['X', 'O', 'X', 'O', 'O']],
    best_groups(Board, LargestX, LargestO),
    format("Largest X group: ~d~nLargest O group: ~d", [LargestX, LargestO]).
