piece_weight(king, 200).
piece_weight(queen, 9).
piece_weight(rook, 5).
piece_weight(knight, 3).
piece_weight(bishop, 3).
piece_weight(pawn, 1).

material_score([], [], 0).
material_score(White, Black, Score) :-
    material_score(White, WhiteScore),
    material_score(Black, BlackScore),
    Score is WhiteScore - BlackScore.

material_score([], 0).
material_score([Piece@_ | Pieces], Score) :-
    material_score(Pieces, TempScore),
    piece_weight(Piece, Weight),
    Score is TempScore + Weight.
