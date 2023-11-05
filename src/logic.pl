player_black('X').
player_white('O').

/* Change player given a current player */
%change_player(+Player, -NewPlayer)
change_player(Player, NewPlayer) :-
    player_black(Player),
    player_white(NewPlayer).

change_player(Player, NewPlayer) :-
    player_white(Player),
    player_black(NewPlayer).

%initial_state(+Size, -GameState)
/* A GameState is a list of 4 elements:
  1. Board
  2. Player
  3. List of free moves available for the current player
  4. List of drop moves available for the current player */
initial_state(Size, GameState) :-
    player_black(Player),
    initialize_board(Size, Board),
    valid_free_moves([Board, Player, [], []], FreeMoves),
    valid_drop_moves([Board, Player, [], []], DropMoves),
    GameState = [Board, Player, FreeMoves, DropMoves].

%move_free(+GameState, +Move, -NewBoard)
move_free(GameState, Move, NewBoard) :-
    GameState = [Board, Player, _, _],
    board_set_element(Board, Move, Player, NewBoard).

%move_drop(+GameState, +Move, -NewBoard)
move_drop(GameState, Move, NewBoard) :-
    GameState = [Board, Player, _, _],
    Move = [[X0, Y0], [X1, Y1]],
    board_set_element(Board, [X0, Y0], Player, TempBoard),
    change_player(Player, Opponent),
    board_set_element(TempBoard, [X1, Y1], Opponent, NewBoard).

/* Get all available free moves given a Board */
%valid_free_moves(+GameState, -ListOfMoves).
valid_free_moves(GameState, ListOfMoves) :-
    GameState = [Board, _, _, _],
    findall([X, Y], valid_free_move(Board, [X, Y]), ListOfMoves).

%valid_free_move(+Board, -Position).
valid_free_move(Board, [X, Y]) :-
    board_get_element(Board, [X, Y], Element),
    Element == ' ',
    board_get_adjacent(Board, [X, Y], ListOfAdjacent),
    check_all_spaces(ListOfAdjacent).

/* Check if all positions are free positions */
%check_all_spaces(+ListAdjacent)
check_all_spaces(ListOfAdjacent) :-
    length(ListOfAdjacent, NumberOfAdjacent),
    check_all_spaces(ListOfAdjacent, NumberOfAdjacent).

%check_all_spaces(+ListOfAdjacent, +N)
check_all_spaces([], 0).
check_all_spaces([[H, _]|T], N) :-
    H == ' ',
    N1 is N - 1,
    check_all_spaces(T, N1).

/* Get all available drop moves given a Board */
/* A drop move is represented by a list of two positions: [[X0, Y0],[X1, Y1]] */
%valid_drop_moves(+GameState, -ListOfMoves).
valid_drop_moves(GameState, ListOfMoves) :-
    GameState = [Board, Player, _, _],
    findall(DropMove, valid_drop_move(Board, Player, DropMove), ListOfMoves).

%valid_drop_move(+Board, +Player, -DropMove).
valid_drop_move(Board, Player, [[X0, Y0], [X1, Y1]]) :-
    board_get_element(Board, [X0, Y0], Element),
    change_player(Player, Opponent),
    Element == Opponent,
    board_get_adjacent(Board, [X0, Y0], ListOfAdjacent),
    member([' ', [X1, Y1]], ListOfAdjacent).

% valid_move(Board, Move), make_move(Board, Move, NewBoard)
greedy_best_move(GameState, Move) :-
    GameState = [Board, Player, FreeMoves, DropMoves],
    (FreeMoves == [] ->
        findall(Value-DropMove-[], (
            member(DropMove, DropMoves),
            value_move(GameState, DropMove-[], Value)
        ), ListOfMoves);
        (DropMoves \== [] -> 
            findall(Value-DropMove-FreeMove, (
                member(DropMove, DropMoves),
                member(FreeMove, FreeMoves),
                valid_move(GameState, DropMove-FreeMove),
                value_move(GameState, DropMove-FreeMove, Value)
            ), ListOfMoves);
            findall(Value-[]-FreeMove, (
                member(FreeMove, FreeMoves),
                value_move(GameState, []-FreeMove, Value)
            ), ListOfMoves)
        )
    ),
    sort(ListOfMoves, TempSortedMoves),
    reverse(TempSortedMoves, SortedList),
    nth0(0, SortedList, BestValue-_-_),
    findall(Value-DropMove-FreeMove, (
        member(Value-DropMove-FreeMove, SortedList), Value == BestValue
    ), BestMovesList),
    length(BestMovesList, N),
    random(0, N, Index),
    nth0(Index, BestMovesList, Move).

valid_move(GameState, DropMove-FreeMove) :-
    GameState = [Board, Player, _, _],
    (DropMove \== [] ->
        move_drop(GameState, DropMove, NewBoard),
        (FreeMove \== [] ->
            valid_free_move(NewBoard, FreeMove)
        );
        valid_free_move(Board, FreeMove)
    ).

value_move(GameState, DropMove-FreeMove, Value) :-
    GameState = [Board, Player, _, _],
    (DropMove \== [] ->
        move_drop(GameState, DropMove, NewBoard),
        (FreeMove \== [] ->
            move_free([NewBoard, Player, [],[]], FreeMove, NewBoard2),
            value([NewBoard2, Player, _, _], Player, Value);
            value([NewBoard, Player, _, _], Player, Value)
        );
        move_free(GameState, FreeMove, NewBoard),
        value([NewBoard, Player, _, _], Player, Value)
    ).

te(Move):-
    Board = [[' ', ' ', ' ', ' '],
             [' ', ' ', ' ', ' '],
             [' ', ' ', ' ', ' '],
             [' ', ' ', ' ', ' ']],
    Player = 'X',
    valid_drop_moves([Board, Player, _, _], DropMoves),
    valid_free_moves([Board, Player, _, _], FreeMoves),
    GameState = [Board, Player, FreeMoves, DropMoves],
    % (greedy_best_move(GameState, Move)).
    valid_move(GameState, Move).

% value(+GameState, +Player, -Value)
value(GameState, Player, Value) :-
    GameState = [Board, _, _, _],
    largest_groups(Board, LargestX, LargestO),
    (Player == 'X' ->
        Value is LargestX - LargestO;
        Value is LargestO - LargestX
    ).

% choose_move(+GameState, +Level, -Move)
% move is a term DropMove-FreeMove
choose_move(GameState, Level, Move) :-
    (Level == 1 ->
        choose_move_level1(GameState, Move);
        choose_move_level2(GameState, Move)
    ).

choose_move_level1(GameState, Move) :-
    GameState = [Board, _, FreeMoves, DropMoves],
    (FreeMoves == [] ->
        findall(DropMove-[], (member(DropMove, DropMoves)), ListOfMoves);
        (DropMoves \== [] -> 
            findall(DropMove-FreeMove, (
                member(DropMove, DropMoves),
                member(FreeMove, FreeMoves),
                valid_move(GameState, DropMove-FreeMove)
            ), ListOfMoves);
            findall([]-FreeMove, (
                member(FreeMove, FreeMoves)
            ), ListOfMoves)
        )
    ),
    length(ListOfMoves, N),
    random(0, N, Index),
    nth0(Index, ListOfMoves, Move).

% choose_free_move_level2(GameState, Move).
choose_move_level2(GameState, DropMove-FreeMove) :-
    greedy_best_move(GameState, Value-DropMove-FreeMove).


% test_game(N) :-
%     initial_state(N, GameState),
%     game_display(GameState),
%     GameState = [Board, Player, _, _],
%     valid_free_moves(GameState, ListOfMoves),
%     write('Valid moves: '),
%     write(ListOfMoves).

% test_game1(N, [X,Y]) :-
%     initialize_board(N, Board),
%     display_board(Board),
%     valid_free_move(Board, [X, Y]).

% test_game2(N) :-
%     initial_state(N, GameState),
%     game_display(GameState),
%     GameState = [Board, Player, FM, DM],
%     valid_free_moves(GameState, ListOfMoves),
%     write('Valid moves: '),
%     write(ListOfMoves),
    
%     player_black(Piece),
%     board_set_element(Board, [1,1], Piece, NewBoard),

%     NewGameState = [NewBoard, Player, FM, DM],
%     game_display(NewGameState),
%     write('Game State: '), 
%     write(NewGameState),nl,

%     valid_free_moves(NewGameState, NewListOfMoves),
%     write('Valid moves: '),
%     write(NewListOfMoves).

% test_game3(N, Move) :-
%     initial_state(N, GameState),
%     GameState = [Board, Player, FM, DM],
%     game_display(GameState),
%     % print writes
%     write('List of free moves: '),
%     write(FM),nl,
%     write('Player: '),
%     write(Player),nl,

%     move_free(GameState, Move, NewGameState),

%     NewGameState = [NewBoard, NewPlayer, NewFM, NewDM],
%     game_display(NewGameState),
%     % print writes
%     write('Free moves: '),
%     write(NewFM),nl,
%     write('Player: '),
%     write(NewPlayer),nl.

% test_game4(N, Move) :-
%     initial_state(N, GameState),
%     GameState = [Board, Player, FM, DM],
%     game_display(GameState),
%     % print writes
%     write('Player: '),
%     write(Player),nl,
%     write('List of free moves: '),
%     write(FM),nl,
%     write('Drop moves: '),
%     write(DM),nl,

%     move_free(GameState, Move, NewGameState),

%     NewGameState = [NewBoard, NewPlayer, NewFM, NewDM],
%     game_display(NewGameState),
%     % print writes
%     write('Player: '),
%     write(NewPlayer),nl,
%     write('Free moves: '),
%     write(NewFM),nl,
%     write('Drop moves: '),
%     write(NewDM),nl.
    
