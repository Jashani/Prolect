:- [legal_moves].
:- [utilities].

representation(white, rook, 'R').
representation(white, knight, 'N').
representation(white, bishop, 'B').
representation(white, queen, 'Q').
representation(white, king, 'K').
representation(white, pawn, 'P').
representation(black, rook, 'r').
representation(black, knight, 'n').
representation(black, bishop, 'b').
representation(black, queen, 'q').
representation(black, king, 'k').
representation(black, pawn, 'p').

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

test_board :-
    pieces3(White, Black, _),
    generate_board(White vs Black, Board),
    print_board(Board).

pieces1(P1, P2) :-  % For testing
    P1 = [pawn@1/8, pawn@2/7, pawn@3/6, pawn@4/5],
    P2 = [pawn@8/8, pawn@7/7, pawn@6/6, pawn@5/5].

pieces2(P1, P2) :-  % For testing
    P1 = [pawn@1/8, pawn@8/8],
    P2 = [pawn@2/8, pawn@5/5].

pieces3(P1, P2, R) :-  % For testing
    P1 = [bishop@5/2, king@2/7, rook@5/6, queen@8/6, pawn@1/7, knight@2/4],
    P2 = [pawn@4/5, bishop@6/2, pawn@4/3, king@3/3, queen@1/8, rook@8/2, rook@4/1],
    R = [queen@1/8, pawn@1/7, knight@3/7, rook@5/6, queen@8/6, pawn@4/5, knight@2/4, 
        pawn@2/3, king@3/3, bishop@5/2, bishop@6/2, rook@8/2, rook@4/1].

% pieces3 board should look like this:
% 8 [q, , , , , , , ]
% 7 [P,K, , , , , , ]
% 6 [ , , , ,R, , ,Q]
% 5 [ , , ,p, , , , ]
% 4 [ ,N, , , , , , ]
% 3 [ , ,k,p, , , , ]
% 2 [ , , , ,B,b, ,r]
% 1 [ , , ,r, , , , ]
%    a b c d e f g h