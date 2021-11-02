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
