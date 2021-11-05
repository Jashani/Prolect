:- [operators].

:- dynamic pawn_count/1.
pawn_count(16).

weight(king, 200).
weight(queen, 9).
weight(bishop, 3).
weight(pawn, 1).
weight(doubled_pawn, (-0.5)).
weight(mobility, 0.1).
weight(bishop_pair, 0.5).
weight(centre_control, 0.1).
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
% Weigh bishop based on pawn structure
% Penalty for blocked pawns
% Bonus for passed pawns
% Bonus for rooks on open files
% Bonus for rooks defending each other
% King safety!
% Centre control
% Rule of the square
% Tempo?

draw() :- fail.
checkmate(_) :- fail.

terminal_score(Score) :-
    draw(),
    Score = 0, !
    ;
    checkmate(white),
    Score = 9999, !
    ;
    checkmate(black),
    Score = -9999, !.

score(White vs Black, Colour, Score) :-
    terminal_score(Score)
    ;
    count_pawns(White, Black),
    material_score(White, Black, MaterialScore),
    pawn_score(White, Black, PawnScore),
    TmpScore is MaterialScore + PawnScore,
    (
        Colour = black, !,
        Score is TmpScore * (-1)
        ;
        Score = TmpScore
    ).
    %format('Score is ~w for~nWhite: ~w~nBlack: ~w~n', [Score, White, Black]), nl.

score(White vs Black, WhiteMoves, BlackMoves, Colour, Score) :-
    terminal_score(Score)
    ;
    count_pawns(White, Black),
    material_score(White, Black, MaterialScore),
    pawn_score(White, Black, PawnScore),
    mobility_score(WhiteMoves, BlackMoves, MobilityScore),
    centre_control_score(WhiteMoves, BlackMoves, CentreScore),
    %format('Material: ~w~nPawns: ~w~nMobility: ~w~n', [MaterialScore, PawnScore, MobilityScore]),
    TmpScore is MaterialScore + PawnScore + MobilityScore + CentreScore,
    (
        Colour = black, !,
        Score is TmpScore * (-1)
        ;
        Score = TmpScore
    ).

centre_control_score(WhiteMoves, BlackMoves, Score) :-
    centre_moves(WhiteMoves, WhiteCentreMoves),
    centre_moves(BlackMoves, BlackCentreMoves),
    length(WhiteCentreMoves, WhiteScore),
    length(BlackCentreMoves, BlackScore),
    weight(centre_control, Weight),
    Score is Weight * (WhiteScore - BlackScore).

centre_moves([], []).
centre_moves([pawn@_ goto _ | Moves], CentreMoves) :-
    !, centre_moves(Moves, CentreMoves).
centre_moves([Piece goto X/Y | Moves], [Piece goto X/Y | CentreMoves]) :-
    X < 6, X > 3, Y < 6, Y > 3, !,
    centre_moves(Moves, CentreMoves).
centre_moves([_ | Moves], CentreMoves) :-
    centre_moves(Moves, CentreMoves).

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
    weight(mobility, Weight),
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

pieces4(White vs Black, WhiteMoves, BlackMoves) :-
    White = [pawn@1/2, pawn@2/2, pawn@3/2, pawn@4/2, pawn@5/4, pawn@6/2, pawn@7/2, pawn@8/2,
            rook@1/1, knight@2/1, bishop@3/1, queen@4/1, king@5/1, bishop@3/4, knight@7/1, rook@8/1],
    Black = [rook@1/8, knight@2/8, bishop@3/8, queen@4/8, king@5/8, bishop@6/8, knight@7/8, rook@8/8,
            pawn@1/7, pawn@2/7, pawn@3/7, pawn@4/7, pawn@5/6, pawn@6/7, pawn@7/7, pawn@8/7],
    valid_moves(White vs Black, white, [nomove], WhiteMoves),
    write('White moves: '), write(WhiteMoves), nl,
    valid_moves(Black vs White, black, [nomove], BlackMoves),
    write('Black moves: '), write(BlackMoves), nl.