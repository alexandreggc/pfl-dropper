player_black('X').
player_white('O').

% initial_state(+Size, -GameState)
% A GameState is a list of 4 elements:
% 1. Board
% 2. Player
% 3. List of free moves available
% 4. List of drop moves available
initial_state(Size, GameState) :-
    player_black(Player),
    initialize_board(Size, Board),
    GameState = [Board, Player, [], []].
    
% display_game(+GameState)
display_game(GameState) :-
    GameState = [Board, _, _, _],
    display_board(Board).

% move(+GameState, +Move, -NewGameState)

% valid_moves(+GameState, +Player, -ListOfMoves).
valid_moves(GameState, Player, ListOfMoves) :-
    GameState = [Board, _, _, _],
    findall([X, Y], valid_free_move(Board, [X, Y]), ListOfMoves).

% valid_free_move(+Board, +Position).
valid_free_move(Board, [X, Y]) :-
    board_get_element(Board, [X, Y], Element),
    Element == ' ',
    board_get_adjacent(Board, [X, Y], ListOfAdjacent),
    check_all_spaces(ListOfAdjacent).

check_all_spaces(ListOfAdjacent) :-
    length(ListOfAdjacent, NumberOfAdjacent),
    check_all_spaces(ListOfAdjacent, NumberOfAdjacent).
check_all_spaces([], 0).
check_all_spaces([[H, _]|T], N) :-
    H == ' ',
    N1 is N - 1,
    check_all_spaces(T, N1).

% game_over(+GameState, -Winner).

% value(+GameState, +Player, -Value)

% choose_move(+GameState, +Player, +Level, -Move)

test_game(N) :-
    initial_state(N, GameState),
    display_game(GameState),
    GameState = [Board, Player, _, _],
    valid_moves(GameState, Player, ListOfMoves),
    write('Valid moves: '),
    write(ListOfMoves).

test_game1(N, [X,Y]) :-
    initialize_board(N, Board),
    display_board(Board),
    valid_free_move(Board, [X, Y]).

test_game2(N) :-
    initial_state(N, GameState),
    display_game(GameState),
    GameState = [Board, Player, FM, DM],
    valid_moves(GameState, Player, ListOfMoves),
    write('Valid moves: '),
    write(ListOfMoves),
    
    player_black(Piece),
    board_set_element(Board, [1,1], Piece, NewBoard),

    NewGameState = [NewBoard, Player, FM, DM],
    display_game(NewGameState),
    write('Game State: '), 
    write(NewGameState),nl,

    valid_moves(NewGameState, Player, NewListOfMoves),
    write('Valid moves: '),
    write(NewListOfMoves).


