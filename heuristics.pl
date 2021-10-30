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

mobility_score([], [], 0).
mobility_score(WhiteMoves, BlackMoves, Score) :-
    length(WhiteMoves, WhiteScore),
    length(BlackMoves, BlackScore),
    Score is WhiteScore - BlackScore.

pawn_score([], [], 0).
pawn_score(White, Black, Score) :-
    pawn_score(White, WhiteScore),
    pawn_score(Black, BlackScore),
    Score is WhiteScore - BlackScore.

pawn_score([], 0).
pawn_score(Pawns, Score) :-
    only_pawns(Pawns, OnlyPawns),
    doubled_pawns(OnlyPawns, DoubledCount),
    Score is (-0.5) * DoubledCount.

% Filters out non-pawn pieces.
only_pawns([], []).
only_pawns([Piece@Position | Pieces], Filtered) :-
    only_pawns(Pieces, TempFiltered),
    (Piece = pawn,
    Filtered = [Piece@Position | TempFiltered], !
    ;
    Filtered = TempFiltered).

% Returns count of doubled pawns. First parameter should be filtered with only_pawns/2.
doubled_pawns([], 0).
doubled_pawns([pawn@X/_ | Rest], DoubledCount) :-
    doubled_pawns(Rest, TempCount),
    (member(pawn@X/_, Rest), !,
    DoubledCount is TempCount + 1
    ;
    DoubledCount = TempCount).