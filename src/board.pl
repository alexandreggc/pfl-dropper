% define start letter code of the board
start_letter(65).

% set up size of board
board_size(5).

% Predicate to initialize a board of size N
initialize_board(N, Board) :-
    create_list_of_rows(N, N, Board).

% Predicate to create a list of N rows, each containing N spaces
create_list_of_rows(0, _, []).
create_list_of_rows(N, M, [Row | Rest]) :-
    N > 0,
    create_row(M, Row),
    N1 is N - 1,
    create_list_of_rows(N1, M, Rest).

% Predicate to create a list of N spaces
create_row(0, []).
create_row(N, [' ' | Rest]) :-
    N > 0,
    N1 is N - 1,
    create_row(N1, Rest).

% Predicate to display the board in a friendly way
display_board(Board) :-
    nl,
    board_size(N),
    write('  '),
    display_numbers(1,N),
    display_rows(Board),
    nl.

% Predicate to display the rows of the board
display_rows(Board) :-
    FisrtLetterCode is 65,
    display_rows_helper(FisrtLetterCode, Board).
display_rows_helper(LetterCode, []) :-
    board_size(N),
    display_horizontal_line(N).
display_rows_helper(LetterCode, [Row | Rest]) :-
    board_size(N),
    display_horizontal_line(N),
    put_code(LetterCode),
    display_row(Row),
    NewLetterCode is LetterCode + 1,
    display_rows_helper(NewLetterCode, Rest).

% Predicate to display a single row
display_row([]) :-
    write(' |'), nl.
display_row([Cell | Rest]) :-
    write(' | '),
    write(Cell),
    display_row(Rest).

% Predicate to display a horizontal line
display_horizontal_line(N) :-
    write('  '),
    display_h_line(N),
    write('*'), nl.

display_h_line(0).
display_h_line(N) :-
    write('*---'),
    N1 is N - 1,
    display_h_line(N1).

% Display header of the board with numbers
display_numbers(N, N) :-
    write('  '),
    write(N),
    nl.
display_numbers(N0, N) :-
    N0 < N,
    write('  '),
    write(N0),
    write(' '),
    N1 is N0 + 1,
    display_numbers(N1, N).


% test to print the board
test_board_1 :-
    board_size(N),
    initialize_board(N, Board),
    write('Board: '), write(Board), nl,
    display_board(Board).
