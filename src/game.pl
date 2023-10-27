player_black(b).
player_white(w).

% initial_state(+Size, -GameState)
initial_state(Size, GameState) :-
    player_black(Player),
    initialize_board(Size, Board),
    FreeMoves is Size * Size,
    GameState = [Board, Size, Player, FreeMoves],
    display_board(Board).
    
% display_game(+GameState)

% move(+GameState, +Move, -NewGameState)

% valid_moves(+GameState, +Player, -ListOfMoves).

% game_over(+GameState, -Winner).

% value(+GameState, +Player, -Value)

% choose_move(+GameState, +Player, +Level, -Move)

