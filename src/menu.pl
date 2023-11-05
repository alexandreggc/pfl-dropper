
% clear/0
% Clears the screen, for better user experience
clear :- write('\e[2J').

% menu_header_format(+Header)
% prints the header of a menu, for example MAIN MENU or INSTRUCTIONS
menu_header_format(Header):-
    format('~n~`*t ~p ~`*t~57|~n', [Header]).

% menu_empty_line/0
% Prints an empty line inside a menu 
menu_empty_line :-
    format('*~t*~57|~n', []).


% menu_sec_header_format(+Label1, +Label2)
% Prints an header with 2 columns for a secundary table 
% Used for readability
menu_sec_header_format(Label1, Label2):-
    format('*~t~a~t~15+~t~a~t~40+~t*~57|~n',
            [Label1, Label2]).


% menu_option_format(+Option, +Details)
% prints the option number and associated details in a menu-like format 
menu_option_format(Option, Details):-
    format('*~t~d~t~15|~t~a~t~40+~t*~57|~n',
          [Option, Details]).

% menu_text_format(+Text)
% Prints a center-aligned text inside a menu 
menu_text_format(Text):-
  format('*~t~a~t*~57|~n', [Text]).

% menu_bottom_line/0
% Prints a row of '*' to end the menu 
menu_bottom_line :-
    format('~`*t~57|~n', []).

% menu/0
% Displays the menu
menu:-
    menu_header_format('DROPPER GAME'),
    menu_empty_line,
    menu_sec_header_format('Option', 'Details'),
    menu_empty_line,
    menu_option_format(1, 'Player vs Player'),
    menu_option_format(2, 'Player vs Computer'),
    menu_option_format(3, 'Computer vs Computer'),
    menu_option_format(4, 'Game Intructions'),
    menu_empty_line,
    menu_option_format(0, 'EXIT'),
    menu_empty_line,
    menu_bottom_line,
    read_number(0, 4, Number),
    menu_option(Number).



% menu_option(+Option)
% Executes the option selected by the user

% Exit Menu
menu_option(0):-
    write('Thank You For Playing').

% Player vs Player
menu_option(1):-
    write('Player vs Player'), nl,
    read_board_size(BoardSize),
    game_start(BoardSize).

menu_option(2):-
    write('Player vs Computer'), nl,
    read_board_size(BoardSize),
    read_ai_level(AILevel),
    read_ai_player(AIPlayer),
    game_start_ai(BoardSize, AILevel, AIPlayer).

menu_option(3):-
    write('Computer vs Computer').

menu_option(4):-
    clear,
    menu_header_format('INSTRUCTIONS'),
    menu_empty_line,
    format('*~t~s~t~30|~t~s,~t~23+~t*~57|~n', ["Player 1", "E1,L1,R1"]),
    format('*~t~s~t~30|~t~s,~t~23+~t*~57|~n', ["Player 2", "E2,L2,R2"]),
    menu_empty_line,
    menu_text_format('The goal of the game is to control 3 of the 4 Goals'),
    menu_text_format('at the end of your turn'),
    menu_empty_line,
    menu_empty_line,
    menu_text_format('-- GENERAL RULES --'),
    menu_empty_line,
    menu_text_format('The game starts with a square checkered board,'),
    menu_text_format('each player controls 6 animals of 3 different types.'),
    menu_text_format('Each piece can move as far as it can'),
    menu_text_format('on its intended line of travel as long'),
    menu_text_format('as its not within the square perimeter,'),
    menu_text_format('orthogonal and diagonal,of the animal its afraid of,'),
    menu_text_format('If your piece is next to a piece,orthogonal and diagonal'),
    menu_text_format('that it is SCARED of it,'),
    menu_text_format('it MUST move on your next turn'),
    menu_text_format('You cannot move a piece next to a piece'),
    menu_text_format('orthogonal and diagonal, that it is SCARED of.'),
    menu_empty_line,
    menu_empty_line,
    menu_text_format('-- PIECES AND THEIR MOVEMENT--'),
    menu_empty_line,
    menu_text_format('The MOUSE can move ORTHOGONALLY,horizontal or vertical'),
    menu_text_format('and is SCARED of the LION.'),
    menu_text_format('The LION can move DIAGONALLY '),
    menu_text_format('and is SCARED of the ELEPHANT.'),
    menu_text_format('The ELEPHANT can move BOTH ORTHOGONALLY and DIAGONALLY '),
    menu_text_format('and is SCARED of the MOUSE.'),
    menu_empty_line,
    menu_empty_line,
    menu_bottom_line,
    menu.
