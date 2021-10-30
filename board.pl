:- [legal_moves].

assign_white(rook, white_rook).
assign_white(knight, white_knight).
assign_white(bishop, white_bishop).
assign_white(queen, white_queen).
assign_white(king, white_king).
assign_white(pawn, white_pawn).
assign_black(rook, black_rook).
assign_black(knight, black_knight).
assign_black(bishop, black_bishop).
assign_black(queen, black_queen).
assign_black(king, black_king).
assign_black(pawn, black_pawn).
representation(white_rook, 'R').
representation(white_knight, 'N').
representation(white_bishop, 'B').
representation(white_queen, 'Q').
representation(white_king, 'K').
representation(white_pawn, 'P').
representation(black_rook, 'r').
representation(black_knight, 'n').
representation(black_bishop, 'b').
representation(black_queen, 'q').
representation(black_king, 'k').
representation(black_pawn, 'p').
representation(p, 'p').

colour_pieces_white([], []).
colour_pieces_white([Piece@Position | Rest], [ColouredPiece@Position | ColouredPieces]) :-
    colour_pieces_white(Rest, ColouredPieces),
    assign_white(Piece, ColouredPiece).

colour_pieces_black([], []).
colour_pieces_black([Piece@Position | Rest], [ColouredPiece@Position | ColouredPieces]) :-
    colour_pieces_black(Rest, ColouredPieces),
    assign_black(Piece, ColouredPiece).

generate_board(White, Black, Board) :-
    colour_pieces_white(White, WhitePieces),
    colour_pieces_black(Black, BlackPieces),
    merge_pieces(WhitePieces, BlackPieces, Pieces),
    fill_board(Pieces, Board).

% Place each piece in Pieces in its place in Board, and fill the rest with ' '
fill_board(Pieces, Board) :-
    TopLeft = 1/8,
    fill_board(Pieces, TopLeft, Board).

% No more pieces
fill_board([], 8/1, [[' ']]) :- !.
fill_board([], 8/Y, [[' '] | Rows]) :-
    !, NewY is Y - 1,
    fill_board([], 1/NewY, Rows).
fill_board([], X/Y, [[' ' | Row] | Rows]) :-
    NewX is X + 1,
    fill_board([], NewX/Y, [Row | Rows]).

% Piece matches coordinates
fill_board([Piece@8/1], 8/1, [[Content]]) :-
    !, representation(Piece, Content).
fill_board([Piece@8/Y | Pieces], 8/Y, [[Content] | Rows]) :-
    !, representation(Piece, Content),
    NewY is Y - 1,
    fill_board(Pieces, 1/NewY, Rows).
fill_board([Piece@X/Y | Pieces], X/Y, [[Content | Row] | Rows]) :-
    !, representation(Piece, Content),
    NewX is X + 1,
    fill_board(Pieces, NewX/Y, [Row | Rows]).

% Piece doesn't match
fill_board([Piece | Pieces], 8/Y, [[' '] | Rows]) :-
    !, NewY is Y - 1,
    fill_board([Piece | Pieces], 1/NewY, Rows).
fill_board([Piece | Pieces], X/Y, [[' ' | Row] | Rows]) :-
    !, NewX is X + 1,
    fill_board([Piece | Pieces], NewX/Y, [Row | Rows]).

merge_pieces([], Pieces, Pieces).
merge_pieces(Pieces, [], Pieces).
merge_pieces([Piece1@X1/Y1 | Rest1], [Piece2@X2/Y2 | Rest2], Result) :-
    (Y1 < Y2;
    Y1 == Y2, X1 > X2),
    merge_pieces([Piece1@X1/Y1 | Rest1], Rest2, TempResult),
    Result = [Piece2@X2/Y2 | TempResult], !
    ;
    merge_pieces(Rest1, [Piece2@X2/Y2 | Rest2], TempResult),
    Result = [Piece1@X1/Y1 | TempResult], !.


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

main :-
    pieces3(White, Black, _),
    generate_board(White, Black, Board),
    print_board(Board).

pieces1(P1, P2) :-  % For testing
    P1 = [pawn@1/8, pawn@2/7, pawn@3/6, pawn@4/5],
    P2 = [pawn@8/8, pawn@7/7, pawn@6/6, pawn@5/5].

pieces2(P1, P2) :-  % For testing
    P1 = [pawn@1/8, pawn@8/8],
    P2 = [pawn@2/8, pawn@5/5].

pieces3(P1, P2, R) :-  % For testing
    P1 = [king@2/7, pawn@1/7, rook@5/6, queen@8/6, knight@2/4, bishop@5/2],
    P2 = [queen@1/8, knight@3/7, pawn@4/5, pawn@2/3, king@3/3, bishop@6/2, rook@8/2, rook@4/1],
    R = [queen@1/8, pawn@1/7, knight@3/7, rook@5/6, queen@8/6, pawn@4/5, knight@2/4, 
        pawn@2/3, king@3/3, bishop@5/2, bishop@6/2, rook@8/2, rook@4/1].