code_number(48,0).
code_number(49,1).
code_number(50,2).
code_number(51,3).
code_number(52,4).
code_number(53,5).
code_number(54,6).
code_number(55,7).
code_number(56,8).
code_number(57,9).

read_free_move(FreeMove) :-
    write('Enter Free move (e.g., A1): '),
    (
        read_str(String), parse_free_move(String, FreeMove) ->
        true, !;
        (write('Invalid input!'), nl, read_free_move(FreeMove))
    ).

% Predicate to read a move from the terminal and parse it
read_drop_move(DropMove) :-
    write('Enter Drop move (e.g., A1-B2): '),
    (
        read_str(String), parse_drop_move(String, DropMove) ->
        true, !;
        (write('Invalid input!'), nl, read_drop_move(DropMove))
    ).

% Predicate to parse a move in the format "A1" and extract destination position
parse_free_move(RawString, FreeMove) :-
    convert_to_uppercase(RawString, String),
    length(String, StringLenght),
    StringLenght = 2,
    nth0(0, String, DestXCode),
    nth0(1, String, DestYCode),
    DestX is DestXCode - 65, DestX >= 0, DestX =< 25,
    DestY is DestYCode - 49, DestY >= 0, DestY =< 8,
    FreeMove = [DestX, DestY].

% Predicate to parse a move in the format "A1-B2" and extract source and destination positions
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

wait_for_enter :-
    write('Press Enter to continue...'),
    put_code(10),get_code(_),
    skip_line.

% read_number(+LowerBound,+UpperBound,-Number)
%used in menus to read inputs between the Lower and Upper Bounds
read_number(LowerBound,UpperBound,Number) :-
    format('| Choose an Option (~d-~d) - ', [LowerBound, UpperBound]),
    get_code(NumberASCII),
    peek_char(Char),
    Char == '\n',
    code_number(NumberASCII, Number),
    Number =< UpperBound, Number >= LowerBound, skip_line.
% If the input is invalid
read_number(LowerBound, UpperBound, Number):-
    write('Not a valid number, try again\n'), skip_line,
    read_number(LowerBound, UpperBound, Number).

read_board_size(BoardSize) :-
    write('| Choose the board size (1-9): '),
    get_code(BoardSizeASCII),
    peek_char(Char),
    Char == '\n',
    code_number(BoardSizeASCII, BoardSize),
    BoardSize =< 9, BoardSize >= 1, skip_line.

read_board_size(BoardSize) :-
    write('Not a valid number, try again\n'), skip_line,
    read_board_size(BoardSize).

read_ai_level(AILevel) :-
    write('| Choose the AI level (1-2): '),
    get_code(AILevelASCII),
    peek_char(Char),
    Char == '\n',
    code_number(AILevelASCII, AILevel),
    AILevel =< 2, AILevel >= 1, skip_line.

read_ai_level(AILevel) :-
    write('Not a valid number, try again\n'), skip_line,
    read_ai_level(AILevel).

read_ai_player(AIPlayer) :-
    write('| Choose the AI player to start (1-yes/2-no): '),
    get_code(AIPlayerASCII),
    peek_char(Char),
    Char == '\n',
    code_number(AIPlayerASCII, AIPlayerNum),
    AIPlayerNum =< 2, AIPlayerNum >= 1,
    (AIPlayerNum == 1 -> AIPlayer = 'X'; AIPlayer = 'O'),
    skip_line.