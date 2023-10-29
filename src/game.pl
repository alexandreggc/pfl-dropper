player_black('X').
player_white('O').

% Change player given a current player
% change_player(+Player, -NewPlayer)
change_player(Player, NewPlayer) :-
    player_black(Player),
    player_white(NewPlayer).
change_player(Player, NewPlayer) :-
    player_white(Player),
    player_black(NewPlayer).


% initial_state(+Size, -GameState)
% A GameState is a list of 4 elements:
% 1. Board
% 2. Player
% 3. List of free moves available
% 4. List of drop moves available
initial_state(Size, GameState) :-
    player_black(Player),
    initialize_board(Size, Board),
    valid_free_moves([Board, Player, [], []], FreeMoves),
    valid_drop_moves([Board, Player, [], []], DropMoves),
    GameState = [Board, Player, FreeMoves, DropMoves].
    
% display_game(+GameState)
display_game(GameState) :-
    GameState = [Board, _, _, _],
    display_board(Board).

% move(+GameState, +Move, -NewGameState)
move_free(GameState, Move, NewGameState) :-
    GameState = [Board, Player, FM, DM],
    board_set_element(Board, Move, Player, NewBoard),
    change_player(Player, NewPlayer),
    TempGameState = [NewBoard, NewPlayer, [], []],
    valid_free_moves(TempGameState, FreeMoves),
    valid_drop_moves(TempGameState, DropMoves),
    NewGameState = [NewBoard, NewPlayer, FreeMoves, DropMoves].

% move_drop(+GameState, +Move, -NewGameState)
move_drop(GameState, Move, NewGameState) :-
    GameState = [Board, Player, FM, DM],
    Move = [[X0, Y0], [X1, Y1]],
    board_set_element(Board, [X0, Y0], Player, NewBoard),
    change_player(Player, NewPlayer),
    board_set_element(NewBoard, [X1, Y1], NewPlayer, NewBoard),
    TempGameState = [NewBoard, NewPlayer, [], []],
    valid_free_moves(TempGameState, FreeMoves),
    valid_drop_moves(TempGameState, DropMoves),
    NewGameState = [NewBoard, NewPlayer, FreeMoves, DropMoves].

% Get all available free moves given a Board 
% valid_free_moves(+GameState, -ListOfMoves).
valid_free_moves(GameState, ListOfMoves) :-
    GameState = [Board, _, _, _],
    findall([X, Y], valid_free_move(Board, [X, Y]), ListOfMoves).

% valid_free_move(+Board, -Position).
valid_free_move(Board, [X, Y]) :-
    board_get_element(Board, [X, Y], Element),
    Element == ' ',
    board_get_adjacent(Board, [X, Y], ListOfAdjacent),
    check_all_spaces(ListOfAdjacent).

% Check if all positions are free positions
% check_all_spaces(+ListAdjacent)
check_all_spaces(ListOfAdjacent) :-
    length(ListOfAdjacent, NumberOfAdjacent),
    check_all_spaces(ListOfAdjacent, NumberOfAdjacent).
check_all_spaces([], 0).
check_all_spaces([[H, _]|T], N) :-
    H == ' ',
    N1 is N - 1,
    check_all_spaces(T, N1).

% Get all available drop moves given a Board
% A drop move is represented by a list of two positions: [[X0, Y0],[X1, Y1]]
% valid_drop_moves(+GameState, -ListOfMoves).
valid_drop_moves(GameState, ListOfMoves) :-
    GameState = [Board, Player, _, _],
    findall(DropMove, valid_drop_move(Board, Player, DropMove), ListOfMoves).

valid_drop_move(Board, Player, [[X0, Y0], [X1, Y1]]) :-
    board_get_element(Board, [X0, Y0], Element),
    change_player(Player, Opponent),
    Element == Opponent,
    board_get_adjacent(Board, [X0, Y0], ListOfAdjacent),
    member([' ', [X1, Y1]], ListOfAdjacent).

% game_over(+GameState, -Winner).

% value(+GameState, +Player, -Value)

% choose_move(+GameState, +Player, +Level, -Move)

test_game(N) :-
    initial_state(N, GameState),
    display_game(GameState),
    GameState = [Board, Player, _, _],
    valid_free_moves(GameState, ListOfMoves),
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
    valid_free_moves(GameState, ListOfMoves),
    write('Valid moves: '),
    write(ListOfMoves),
    
    player_black(Piece),
    board_set_element(Board, [1,1], Piece, NewBoard),

    NewGameState = [NewBoard, Player, FM, DM],
    display_game(NewGameState),
    write('Game State: '), 
    write(NewGameState),nl,

    valid_free_moves(NewGameState, NewListOfMoves),
    write('Valid moves: '),
    write(NewListOfMoves).

test_game3(N, Move) :-
    initial_state(N, GameState),
    GameState = [Board, Player, FM, DM],
    display_game(GameState),
    % print writes
    write('List of free moves: '),
    write(FM),nl,
    write('Player: '),
    write(Player),nl,

    move_free(GameState, Move, NewGameState),

    NewGameState = [NewBoard, NewPlayer, NewFM, NewDM],
    display_game(NewGameState),
    % print writes
    write('Free moves: '),
    write(NewFM),nl,
    write('Player: '),
    write(NewPlayer),nl.

test_game4(N, Move) :-
    initial_state(N, GameState),
    GameState = [Board, Player, FM, DM],
    display_game(GameState),
    % print writes
    write('Player: '),
    write(Player),nl,
    write('List of free moves: '),
    write(FM),nl,
    write('Drop moves: '),
    write(DM),nl,

    move_free(GameState, Move, NewGameState),

    NewGameState = [NewBoard, NewPlayer, NewFM, NewDM],
    display_game(NewGameState),
    % print writes
    write('Player: '),
    write(NewPlayer),nl,
    write('Free moves: '),
    write(NewFM),nl,
    write('Drop moves: '),
    write(NewDM),nl.
    
