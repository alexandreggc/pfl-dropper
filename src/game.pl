game_start(N) :-
    initial_state(N, GameState),
    game_loop(GameState).

game_start_ai(N, Level, AIPlayer) :-
    initial_state(N, GameState),
    game_loop_ai(GameState, Level, AIPlayer).

game_loop(GameState) :-
    clear,
    game_display(GameState),
    game_step(GameState, NewGameState),
    (game_over(NewGameState) ->
        game_winner(NewGameState, Winner), !;
        game_loop(NewGameState)
    ).

game_loop_ai(GameState, Level, AIPlayer) :-
    clear,
    game_display(GameState),
    game_step_ai(GameState, Level, AIPlayer, NewGameState),
    wait_for_enter,
    (game_over(NewGameState) ->
        game_winner(NewGameState, Winner), !;
        game_loop_ai(NewGameState, Level, AIPlayer)
    ).

% game_display(+GameState)
game_display(GameState) :-
    GameState = [Board, Player, FreeMove, DropMoves],
    display_board(Board),
    write('Player: '), write(Player), nl.
    % write('Free Moves: '), nl,
    % write(FreeMove), nl,
    % write('Drop Moves: '), nl,
    % write(DropMoves), nl.


% game_step(+GameState, -NewGameState)
game_step(GameState, NewGameState) :-
    GameState = [_, Player, _, DropMoves],

    % player movements
    game_drop_move(GameState, TempBoard),
    valid_free_moves([TempBoard, Player, [], []], FreeMoves),
    TempGameState = [TempBoard, Player, FreeMoves, []],
    %game_display(TempGameState),
    game_free_move(TempGameState, NewBoard),

    change_player(Player, NewPlayer),
    valid_free_moves([NewBoard, NewPlayer, [], []], NewFreeMoves),
    valid_drop_moves([NewBoard, NewPlayer, [], []], NewDropMoves),
    NewGameState = [NewBoard, NewPlayer, NewFreeMoves, NewDropMoves].

game_step_ai(GameState, Level, AIPlayer, NewGameState) :-
    GameState = [Board, Player, _, DropMoves],
    (Player \== AIPlayer ->
        % Human Player
        game_step(GameState, NewGameState);
        % AI Player
        length(DropMoves, DropMovesLength),
        (DropMovesLength \== 0->
            % If there are drop moves available, choose one
            choose_drop_move(GameState, Level, DropMove),
            move_drop(GameState, DropMove, TempBoard),
            valid_free_moves([TempBoard, Player, [], []], FreeMoves),
            TempGameState = [TempBoard, Player, FreeMoves, []];
            TempGameState = GameState
        ),
        length(FreeMoves, FreeMovesLength),
        (FreeMovesLength \== 0 ->
            choose_free_move(TempGameState, Level, FreeMove),
            move_free(TempGameState, FreeMove, NewBoard);
            NewBoard = TempBoard
        ),
        change_player(Player, NewPlayer),
        valid_drop_moves([NewBoard, NewPlayer, [], []], NewDropMoves),
        valid_free_moves([NewBoard, NewPlayer, [], []], NewFreeMoves),
        NewGameState = [NewBoard, NewPlayer, NewFreeMoves, NewDropMoves]
    ).

game_over(GameState) :-
    GameState = [_, _, FreeMoves, DropMoves],
    FreeMoves == [], DropMoves == [].

% game_winner(+GameState, -Winner).
game_winner(GameState, Winner) :-
    GameState = [Board, _, _, _],
    game_display(GameState),
    write('Game Over!'),nl,
    largest_groups(Board, LargestXClusterSize, LargestOClusterSize),
    write('Largest X group size: '), write(LargestXClusterSize), nl,
    write('Largest O group size: '), write(LargestOClusterSize), nl,
    (LargestXClusterSize > LargestOClusterSize ->
        Winner = 'X';
        Winner = 'O'
    ),
    (LargestOClusterSize == LargestXClusterSize ->
        write('Draw!'), nl;
        write('Winner: '), write(Winner), nl
    ).
    

% game_free_move(+GameState, -NewBoard)
game_free_move(GameState, NewBoard) :-
    GameState = [Board, _, FreeMoves, _],
    (( FreeMoves \== [] ) ->
        (read_free_move(FreeMove), member(FreeMove, FreeMoves) ->
            move_free(GameState, FreeMove, NewBoard);
            write('Invalid move!'), nl,
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
            write('Invalid move!'), nl,
            game_drop_move(GameState, NewBoard),!
        );
        NewBoard = Board
    ).
