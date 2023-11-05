
%clear/0
/* Clears the screen, for better user experience */
clear :- write('\e[2J').

%menu_header_format(+Header)
/* Prints the header of a menu, for example MAIN MENU or INSTRUCTIONS */
menu_header_format(Header):-
    format('~n~`*t ~p ~`*t~57|~n', [Header]).

%menu_empty_line/0
/* Prints an empty line inside a menu */
menu_empty_line :-
    format('*~t*~57|~n', []).

%menu_sec_header_format(+Label1, +Label2)
/* Prints an header with 2 columns for a secundary table */
/* Used for readability */
menu_sec_header_format(Label1, Label2):-
    format('*~t~a~t~15+~t~a~t~40+~t*~57|~n', [Label1, Label2]).

%menu_option_format(+Option, +Details)
/* prints the option number and associated details in a menu-like format */
menu_option_format(Option, Details):-
    format('*~t~d~t~15|~t~a~t~40+~t*~57|~n', [Option, Details]).

%menu_text_format(+Text)
/* Prints a center-aligned text inside a menu */
menu_text_format(Text):-
    format('*~t~a~t*~57|~n', [Text]).

%menu_bottom_line/0
/* Prints a row of '*' to end the menu */
menu_bottom_line :-
    format('~`*t~57|~n', []).

%menu/0
/* Displays the menu */
menu:-
    menu_header_format('DROPPER GAME'),
    menu_empty_line,
    menu_sec_header_format('Option', 'Details'),
    menu_empty_line,
    menu_option_format(1, 'Player vs Player'),
    menu_option_format(2, 'Player vs Computer'),
    menu_option_format(3, 'Computer vs Computer'),
    menu_empty_line,
    menu_option_format(0, 'EXIT'),
    menu_empty_line,
    menu_bottom_line,
    read_number(0, 3, Number),
    menu_option(Number).

%menu_option(+Option)
/* Executes the option selected by the user */

/* Exit Menu */
menu_option(0):-
    write('Thank You For Playing').

/* Player vs Player */
menu_option(1):-
    write('Player vs Player'), nl,
    read_board_size(BoardSize),
    game_start(BoardSize),
    wait_for_enter, clear, menu.

/* Player vs Computer */
menu_option(2):-
    write('Player vs Computer'), nl,
    read_board_size(BoardSize),
    read_ai_level(AILevel),
    read_ai_player(AIPlayer),
    game_start_AI(BoardSize, AILevel, AIPlayer),
    wait_for_enter, clear, menu.

/* Computer vs Computer */
menu_option(3):-
    write('Computer vs Computer'),nl,
    read_board_size(BoardSize),
    read_ai_level(AIPlayerXLevel, 'X'),
    read_ai_level(AIPlayerOLevel, 'O'),
    game_start_AIxAI(BoardSize, AIPlayerXLevel, AIPlayerOLevel),
    wait_for_enter, clear, menu.

