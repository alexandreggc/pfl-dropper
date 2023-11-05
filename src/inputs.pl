/* Module to handle user inputs */
/* Predicate to read a free move from the terminal and parse it */
%read_free_move(+FreeMove)
read_free_move(FreeMove) :-
    write('Enter Free move (e.g., A1): '),
    (
        read_str(String), parse_free_move(String, FreeMove) ->
        true, !;
        (write('Invalid input!'), nl, read_free_move(FreeMove))
    ).

/* Predicate to read a drop move from the terminal and parse it */
%read_drop_move(+DropMove)
read_drop_move(DropMove) :-
    write('Enter Drop move (e.g., A1-B2): '),
    (
        read_str(String), parse_drop_move(String, DropMove) ->
        true, !;
        (write('Invalid input!'), nl, read_drop_move(DropMove))
    ).

/* Predicate to parse a move in the format "A1" and extract destination position */
%parse_free_move(+RawString, -FreeMove)
parse_free_move(RawString, FreeMove) :-
    convert_to_uppercase(RawString, String),
    length(String, StringLenght),
    StringLenght = 2,
    nth0(0, String, DestXCode),
    nth0(1, String, DestYCode),
    DestX is DestXCode - 65, DestX >= 0, DestX =< 25,
    DestY is DestYCode - 49, DestY >= 0, DestY =< 8,
    FreeMove = [DestX, DestY].

/* Predicate to parse a move in the format "A1-B2" and extract source and destination positions */
%parse_drop_move(+RawString, -DropMove)
parse_drop_move(RawString, DropMove) :-
    convert_to_uppercase(RawString, String),
    length(String, StringLength),
    StringLength = 5,
    nth0(0, String, SourceXCode),
    nth0(1, String, SourceYCode),
    nth0(2, String, SeparatorCode),
    nth0(3, String, DestXCode),
    nth0(4, String, DestYCode),
    char_code('-', SeparatorCode),
    SourceX is SourceXCode - 65, SourceX >= 0, SourceX =< 25, % Convert 'A' to 0, 'B' to 1, etc.
    DestX is DestXCode - 65, DestX >= 0, DestX =< 25,
    SourceY is SourceYCode - 49, SourceY >= 0, SourceY =< 8, % Convert '1' to 0, '2' to 1, etc.
    DestY is DestYCode - 49, DestY >= 0, DestY =< 8,
    Source = [SourceX, SourceY],
    Destination = [DestX, DestY],
    DropMove = [Source, Destination].

%wait_for_enter
wait_for_enter :-
    write('Press Enter to continue...'),
    put_code(10),get_code(_),
    skip_line.
