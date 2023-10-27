% Predicate to read a move from the terminal and parse it using get_code and peek_char
read_move(Source, Destination) :-
    write('Enter your move (e.g., A1-B2): '),
    (
        read_str(Move), parse_move(Move, Source, Destination) ->
        true, !;
        (write('Invalid input!'), nl, read_move(Source, Destination))
    ),
    (
        check_move(SourceXCode, SourceYCode, DestXCode, DestYCode) -> 
        true, !;
        (write('Invalid move!'), nl, read_move(Source, Destination))
    ).

% Predicate to parse a move in the format "A1-B2" and extract source and destination positions
parse_move(RawMove, Source, Destination) :-
    convert_to_uppercase(RawMove, Move),
    length(Move, MoveLenght),
    MoveLenght = 5,
    nth0(0, Move, SourceXCode),
    nth0(1, Move, SourceYCode),
    nth0(2, Move, SeparatorCode),
    nth0(3, Move, DestXCode),
    nth0(4, Move, DestYCode),
    char_code('-', SeparatorCode),
    SourceX is SourceXCode - 65, SourceX >= 0, SourceX =< 25, % Convert 'A' to 0, 'B' to 1, etc.
    DestX is DestXCode - 65, DestX >= 0, DestX =< 25,
    SourceY is SourceYCode - 48, SourceY >= 1, SourceY =< 9, % Convert '1' to 0, '2' to 1, etc.
    DestY is DestYCode - 48, DestY >= 1, DestY =< 9,
    Source = [SourceX, SourceY],
    Destination = [DestX, DestY].

% predicate to valid the move 
check_move(SourceXCode, SourceYCode, DestXCode, DestYCode).


