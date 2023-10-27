% print a string to the terminal
print_str([]).
print_str([H|T]) :- put_code(H), print_str(T).

% read a string from the terminal
read_str(S) :-
    get_code(C),
    read_str(C, S, []).
read_str(10, S, S) :- !.
read_str(C, S, L) :-
    append(L, [C], L1),
    get_code(C1),
    read_str(C1, S, L1).

% convert any string to an uppercase string
% convert_to_uppercase(+Lower, -Upper)
convert_to_uppercase([], []).
convert_to_uppercase([LowercaseCode | RestLower], [UppercaseCode | RestUpper]) :-
    (LowercaseCode >= 97, LowercaseCode =< 122 -> 
        UppercaseCode is LowercaseCode - 32 ; 
        UppercaseCode = LowercaseCode),
    convert_to_uppercase(RestLower, RestUpper).

% Predicate to get adjacent positions in an N x N board where indices start at 0
get_adjacent_positions(N, [X, Y], AdjacentPositions) :-
    findall([NewX, NewY], (
        delta(DeltaX, DeltaY),
        NewX is X + DeltaX,
        NewY is Y + DeltaY,
        valid_position(N, NewX, NewY)
    ), AdjacentPositions).

% Predicate to check if a position is within the board boundaries
valid_position(N, X, Y) :-
    X >= 0, X < N,
    Y >= 0, Y < N.

% Define the delta values for adjacent positions (8-connectivity)
delta(1, 0).
delta(0, 1).
delta(-1, 0).
delta(0, -1).
delta(1, 1).
delta(-1, 1).
delta(1, -1).
delta(-1, -1).
