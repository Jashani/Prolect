weight(king, 200).
weight(queen, 9).
weight(bishop, 3).
weight(pawn, 1).

weight(doubled_pawn, (-0.5)).
weight(mobility, 0.1).

pawn_count(16).
weight(knight, Weight) :-
    BaseWeight = 2.5,
    pawn_count(PawnCount),
    PawnBonus is PawnCount/16,
    Weight is BaseWeight + PawnBonus.
weight(rook, Weight) :-
    BaseWeight = 5.5,
    pawn_count(PawnCount),
    PawnPenalty is PawnCount/16,
    Weight is BaseWeight - PawnPenalty.

% Potential evaluations:
% Add weight to bishop pair
% Weigh bishop based on pawn structure
% Penalty for blocked pawns
% Bonus for rooks on open files
% Bonus for rooks defending each other
% King safety!
% Centre control
% Rule of the square

score([], [], 0).
score(White, Black, Score) :-
    material_score(White, Black, MaterialScore),
    pawn_score(White, Black, PawnScore),
    Score is MaterialScore + PawnScore.

score([], [], [], [], 0).
score(White, Black, WhiteMoves, BlackMoves, Score) :-
    material_score(White, Black, MaterialScore),
    pawn_score(White, Black, PawnScore),
    mobility_score(WhiteMoves, BlackMoves, MobilityScore),
    Score is MaterialScore + PawnScore + MobilityScore.

material_score([], [], 0).
material_score(White, Black, Score) :-
    material_score(White, WhiteScore),
    material_score(Black, BlackScore),
    Score is WhiteScore - BlackScore.

material_score([], 0).
material_score([Piece@_ | Pieces], Score) :-
    material_score(Pieces, TempScore),
    weight(Piece, Weight),
    Score is TempScore + Weight.

mobility_score([], [], 0).
mobility_score(WhiteMoves, BlackMoves, Score) :-
    length(WhiteMoves, WhiteScore),
    length(BlackMoves, BlackScore),
    weight(mobility, Weight)
    Score is Weight * (WhiteScore - BlackScore).

pawn_score([], [], 0).
pawn_score(White, Black, Score) :-
    pawn_score(White, WhiteScore),
    pawn_score(Black, BlackScore),
    Score is WhiteScore - BlackScore.

pawn_score([], 0).
pawn_score(Pawns, Score) :-
    only_pawns(Pawns, OnlyPawns),
    doubled_pawns(OnlyPawns, DoubledCount),
    weight(doubled_pawn, Weight),
    Score is Weight * DoubledCount.

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