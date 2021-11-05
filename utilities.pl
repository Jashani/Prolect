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

% is_en_passant(+OpponentPieces, +Move, -VirtualPawnPosition)
% Checks if Move describes a pawn taking another pawn through "en passant",
% and returns the position of the taken pawn.
% Assumes Move is a valid move.
is_en_passant(OpponentPieces, pawn@X1/Y1 goto X2/Y2, X2/Y1) :-
    X1 =\= X2,
    \+ position_taken(X2/Y2, OpponentPieces).

% is_promotion(+Move)
% Checks if Move describes a pawn getting to the last rank and promotes.
% Assumes Move is a valid move.
is_promotion(pawn@_/_ goto _/Y2) :-
    Y2 = 1 ; Y2 = 8.

% apply_move(+(PlayerPieces vs OpponentPieces), +Move, -(NewPlayerPieces vs NewOpponentPieces))
% Make appropriate changes in player piece lists according to move.
apply_move(PlayerPieces vs OpponentPieces, pawn@Origin goto Position, NewPlayerPieces vs NewOpponentPieces) :-
    is_promotion(pawn@Origin goto Position), !,
    remove_piece(pawn@Origin, PlayerPieces, TempPlayerPieces),
    insert_piece(queen@Position, TempPlayerPieces, NewPlayerPieces),
    remove_piece(pawn@Position, OpponentPieces, NewOpponentPieces), !.

apply_move(PlayerPieces vs OpponentPieces, pawn@Origin goto Position, NewPlayerPieces vs NewOpponentPieces) :-
    is_en_passant(OpponentPieces, pawn@Origin goto Position, PawnRealPosition), !,
    remove_piece(pawn@Origin, PlayerPieces, TempPlayerPieces),
    insert_piece(pawn@Position, TempPlayerPieces, NewPlayerPieces),
    remove_piece(pawn@PawnRealPosition, OpponentPieces, NewOpponentPieces), !.

apply_move(PlayerPieces vs OpponentPieces, Type@Origin goto Position, NewPlayerPieces vs NewOpponentPieces) :-
    remove_piece(Type@Origin, PlayerPieces, TempPlayerPieces),
    insert_piece(Type@Position, TempPlayerPieces, NewPlayerPieces),
    remove_piece(_@Position, OpponentPieces, NewOpponentPieces), !.
