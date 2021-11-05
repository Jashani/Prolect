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

% apply_move(+(PlayerPieces vs OpponentPieces), +Move, -(NewPlayerPieces vs NewOpponentPieces))
% Make appropriate changes in player piece lists according to move.
apply_move(PlayerPieces vs OpponentPieces, pawn@Origin goto Position, NewPlayerPieces vs NewOpponentPieces) :-
    is_en_passant(OpponentPieces, pawn@Origin goto Position, PawnRealPosition), !,
    remove_piece(pawn@Origin, PlayerPieces, TempPlayerPieces),
    insert_piece(pawn@Position, TempPlayerPieces, NewPlayerPieces),
    remove_piece(pawn@PawnRealPosition, OpponentPieces, NewOpponentPieces), !.

apply_move(PlayerPieces vs OpponentPieces, Type@Origin goto Position, NewPlayerPieces vs NewOpponentPieces) :-
    remove_piece(Type@Origin, PlayerPieces, TempPlayerPieces),
    insert_piece(Type@Position, TempPlayerPieces, NewPlayerPieces),
    remove_piece(_@Position, OpponentPieces, NewOpponentPieces), !.
