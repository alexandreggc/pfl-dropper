game_start(N, GameState) :-
    initial_state(N, GameState),
    game_loop(GameState).

game_loop(GameState) :-
    game_display(GameState),
    game_step(GameState, NewGameState),
    (check_game_over(GameState) ->
        game_over(GameState, Winner), !;
        game_loop(NewGameState)
    ).

% game_display(+GameState)
game_display(GameState) :-
    GameState = [Board, Player, FreeMove, DropMoves],
    display_board(Board),
    write('Player: '), write(Player), nl,
    write('Free Moves: '), nl,
    write(FreeMove), nl,
    write('Drop Moves: '), nl,
    write(DropMoves), nl.


% game_step(+GameState, -NewGameState)
game_step(GameState, NewGameState) :-
    GameState = [_, Player, _, DropMoves],

    % player movements
    game_free_move(GameState, TempBoard),
    TempGameState = [TempBoard, Player, [], DropMoves],
    %game_display(TempGameState),
    game_drop_move(TempGameState, NewBoard),

    change_player(Player, NewPlayer),
    valid_free_moves([NewBoard, NewPlayer, [], []], NewFreeMoves),
    valid_drop_moves([NewBoard, NewPlayer, [], []], NewDropMoves),
    NewGameState = [NewBoard, NewPlayer, NewFreeMoves, NewDropMoves].

check_game_over(GameState) :-
    GameState = [_, _, FreeMoves, DropMoves],
    FreeMoves == [], DropMoves == [].

% game_over(+GameState, -Winner).
game_over(GameState, Winner) :-
    GameState = [Board, _, _, _],
    game_display(GameState),
    write('Game Over!'),nl,
    largest_groups(Board, LargestXClusterSize, LargestOClusterSize),
    write('Largest X cluster size: '), write(LargestXClusterSize), nl,
    write('Largest O cluster size: '), write(LargestOClusterSize), nl,
    (LargestXClusterSize > LargestOClusterSize ->
        Winner = 'X';
        Winner = 'O'
    ),
    (LargestOClusterSize == LargestXClusterSize ->
        Winner = 'Draw'),
    nl,nl,
    write('Winner: '), write(Winner), nl.

% game_free_move(+GameState, -NewBoard)
game_free_move(GameState, NewBoard) :-
    GameState = [Board, _, FreeMoves, _],
    (( FreeMoves \== [] ) ->
        (read_free_move(FreeMove), member(FreeMove, FreeMoves) ->
            move_free(GameState, FreeMove, NewBoard);
            game_free_move(GameState, NewBoard),!
        );
        NewBoard = Board
    ).

% game_drop_move(+GameState, -NewBoard)
game_drop_move(GameState, NewBoard) :-
    GameState = [Board, _, _, DropMoves],
    (( DropMoves \== [] ) -> 
        (read_drop_move(DropMove), member(DropMove, DropMoves) ->
            move_drop(GameState, DropMove, NewBoard);
            game_drop_move(GameState, NewBoard),!
        );
        NewBoard = Board
    ).
