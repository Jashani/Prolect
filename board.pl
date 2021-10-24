
print_board([R8,R7,R6,R5,R4,R3,R2,R1]) :-
    nl,
    write('8 '), write(R8), nl,
    write('7 '), write(R7), nl,
    write('6 '), write(R6), nl,
    write('5 '), write(R5), nl,
    write('4 '), write(R4), nl,
    write('3 '), write(R3), nl,
    write('2 '), write(R2), nl,
    write('1 '), write(R1), nl,
    write('   a b c d e f g h'), nl,
    nl.

initial_board(Board) :-
    Board = [['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r'],
             ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p'],
             [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
             [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
             [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
             [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
             ['P', 'P', 'P', 'P', 'P', 'P', 'P', 'P'],
             ['R', 'N', 'B', 'Q', 'K', 'B', 'N', 'R']].

main :-
    initial_board(Board),
    print_board(Board).
