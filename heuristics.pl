:- [operators].

% Maintain pawn count
:- dynamic pawn_count/1.
pawn_count(16).

% Weight of heuristics
weight(king, 200).
weight(queen, 9).
weight(bishop, 3).
weight(pawn, 1).
weight(doubled_pawn, (-0.5)).
weight(mobility, 0.1).
weight(bishop_pair, 0.5).
weight(centre_control, 0.1).
weight(initiative, 0.2).
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

% Calculate scores based on our defined heuristics
score(White vs Black, WhiteMoves, BlackMoves, Colour, Score) :-
    count_pawns(White, Black),
    material_score(White, Black, MaterialScore),
    pawn_score(White, Black, PawnScore),
    mobility_score(WhiteMoves, BlackMoves, MobilityScore),
    centre_control_score(WhiteMoves, BlackMoves, CentreScore),
    weight(initiative, InitiativeScore),
    TmpScore is MaterialScore + PawnScore + MobilityScore + CentreScore + InitiativeScore,
    (
        Colour = black, !,
        Score is TmpScore * (-1)
        ;
        Score = TmpScore
    ).

% Score for minor and major pieces controlling the centre (exluding pawns)
centre_control_score(WhiteMoves, BlackMoves, Score) :-
    centre_moves(WhiteMoves, WhiteCentreMoves),
    centre_moves(BlackMoves, BlackCentreMoves),
    length(WhiteCentreMoves, WhiteScore),
    length(BlackCentreMoves, BlackScore),
    weight(centre_control, Weight),
    Score is Weight * (WhiteScore - BlackScore).

% Finds moves which are in the centre of the board
centre_moves([], []).
centre_moves([pawn@_ goto _ | Moves], CentreMoves) :-
    !, centre_moves(Moves, CentreMoves).
centre_moves([Piece goto X/Y | Moves], [Piece goto X/Y | CentreMoves]) :-
    X < 6, X > 3, Y < 6, Y > 3, !,
    centre_moves(Moves, CentreMoves).
centre_moves([_ | Moves], CentreMoves) :-
    centre_moves(Moves, CentreMoves).

% Total amount of pawns in the game
count_pawns(White, Black) :-
    only_pawns(White, WhitePawns),
    only_pawns(Black, BlackPawns),
    length(WhitePawns, WhitePawnCount),
    length(BlackPawns, BlackPawnCount),
    PawnCount is WhitePawnCount + BlackPawnCount,
    (pawn_count(PawnCount)
    ;
    retract(pawn_count(_)),
    assert(pawn_count(PawnCount))).

% Score based on pieces
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

% Score based on available moves
mobility_score([], [], 0).
mobility_score(WhiteMoves, BlackMoves, Score) :-
    length(WhiteMoves, WhiteScore),
    length(BlackMoves, BlackScore),
    weight(mobility, Weight),
    Score is Weight * (WhiteScore - BlackScore).

% Score based on pawn structure
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

% A bishop pair gives you an advantage!
bishop_pair_score(WhitePieces, BlackPieces, Score) :-
    bishop_pair_score(WhitePieces, WhiteScore),
    bishop_pair_score(BlackPieces, BlackScore),
    Score is WhiteScore - BlackScore.

bishop_pair_score(Pieces, Score) :-
    appears_twice(bishop, Pieces),
    weight(bishop_pair, Score), !
    ;
    Score = 0.

% True if piece of type Piece appears at least twice in the given list.
appears_twice(Piece, [Piece@_ | Rest]) :-
    member(Piece@_, Rest).
appears_twice(Piece, [_ | Rest]) :-
    appears_twice(Piece, Rest).
