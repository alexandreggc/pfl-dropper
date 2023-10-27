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
