:- [legal_moves].
:- [utilities].
:- [configurations].

% Assign each piece its coloured representation.
colour_pieces(_, [], []).
colour_pieces(Colour, [Piece@Position | Rest], [ColouredPiece@Position | ColouredPieces]) :-
    colour_pieces(Colour, Rest, ColouredPieces),
    representation(Colour, Piece, ColouredPiece).

generate_board(White vs Black, Board) :-
    colour_pieces(white, White, WhitePieces),
    colour_pieces(black, Black, BlackPieces),
    merge_pieces(WhitePieces, BlackPieces, Pieces),
    fill_board(Pieces, Board).

% Place each piece in Pieces in its place in Board, and fill the rest with ' '
fill_board(Pieces, Board) :-
    fill_board(Pieces, 8, Board), !. % Start at the top row

% Return a list of rows filled with their relevant pieces.
fill_board(_, 0, []) :- !. % Stop after passing the bottom row.
fill_board(Pieces, CurrentRow, [Row | Rows]) :-
    NewRow is CurrentRow - 1,
    fill_board(Pieces, NewRow, Rows),
    fill_row(Pieces, 1/CurrentRow, Row), !.

% Fill a row with its relevant pieces.
fill_row(_, 9/_, []) :- !. % Stop after passing the rightmost square.
fill_row(Pieces, X/Y, [Piece | Row]) :-
    NewX is X + 1,
    fill_row(Pieces, NewX/Y, Row),
    piece_in_square(Pieces, X/Y, Piece), !.

% Find Piece in given Square.
piece_in_square([], _, ' ') :- !.
piece_in_square([Piece@Square | _], Square, Piece) :- !.
piece_in_square([_ | Rest], Square, Piece) :-
    piece_in_square(Rest, Square, Piece), !.

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
