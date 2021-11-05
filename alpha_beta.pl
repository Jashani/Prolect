:- [heuristics].
:- [legal_moves].
:- [utilities].
:- [operators].

switch_colour(white, black).
switch_colour(black, white).

% Alphabeta algorithm as presented in the book, along with some naming improvements,
% as well as memory access in bound_best to avoid unnecessary calculations.

% alphabeta(+Depth, +Pieces, +Colour, +PreviousMoves, +Alpha, +Beta, -GoodMove, -Score).
alphabeta(Depth, Player vs Opponent, Colour, [LastMove | Rest], Alpha, Beta, GoodMove, Score) :-
    valid_moves(Player vs Opponent, Colour, LastMove, Moves), !,
    (
        Depth > 0,
        bound_best(Depth, Player vs Opponent, Colour, [LastMove | Rest], Moves, Alpha, Beta, GoodMove, Score)
        ;
        switch_colour(Colour, NewColour),
        valid_moves(Opponent vs Player, NewColour, LastMove, OpponentMoves),
        score(Player vs Opponent, Moves, OpponentMoves, Colour, Score) % The static value of the position
    ).

% bound_best(+Depth, +Pieces, +Colour, +PreviousMoves, +Moves, +Alpha, +Beta, -GoodMove, -GoodScore)
bound_best(Depth, Pieces, Colour, PreviousMoves, [Move | Moves], Alpha, Beta, GoodMove, GoodScore) :-
    apply_move(Pieces, Move, NewPlayer vs NewOpponent),
    switch_colour(Colour, NewColour),
    NewDepth is Depth - 1,

    alphabeta(NewDepth, NewOpponent vs NewPlayer, NewColour, [Move | PreviousMoves], Alpha, Beta, _, Score),
    good_enough(Depth, Pieces, Colour, PreviousMoves, Moves, Alpha, Beta, Move, Score, GoodMove, GoodScore).

% good_enough(+Depth, +Pieces, +Colour, +PreviousMoves, +Moves, +Alpha, +Beta, +Move, +Score, -GoodMove, -GoodScore)
good_enough(Depth, _, _, _, [], _, _, Move, Score, Move, Score) :- !.

good_enough(Depth, _, Colour, _, _, Alpha, Beta, Move, Score, Move, Score) :-
    switch_colour(Colour, NewColour),

    (
        min_to_move(NewColour), Score > Beta, ! % Maximizer attained upper bound
        ;
        max_to_move(NewColour), Score < Alpha, ! % Minimizer attained lower bound
    ).

good_enough(Depth, Pieces, Colour, PreviousMoves, Moves, Alpha, Beta, Move, Score, GoodMove, GoodScore) :-
    switch_colour(Colour, NewColour),
    new_bounds(Alpha, Beta, NewColour, Score, NewAlpha, NewBeta), % Refine bounds
    bound_best(Depth, Pieces, Colour, PreviousMoves, Moves, NewAlpha, NewBeta, CandidateMove, CandidateScore),
    better_of(Move, NewColour, Score, CandidateMove, CandidateScore, GoodMove, GoodScore).

new_bounds(Alpha, Beta, Colour, Score, Score, Beta) :-
    min_to_move(Colour), Score > Alpha, !. 

new_bounds(Alpha, Beta, Colour, Score, Alpha, Score) :-
    max_to_move(Colour), Score < Beta, !.

new_bounds(Alpha, Beta, _, _, Alpha, Beta). % Otherwise bounds unchanged

better_of(Move, Colour, Score, _, CandidateScore, Move, Score) :- % Move better than Candidate
    min_to_move(Colour), % Black to move after Move
    Score > CandidateScore, ! % White prefers the greater value
    ;
    max_to_move(Colour), % White to move after Move
    Score < CandidateScore, !. % Black prefers the lesser value

better_of(_, _, _, CandidateMove, CandidateScore, CandidateMove, CandidateScore).  % Otherwise Candidate is better

max_to_move(white). % It's white's turn.

min_to_move(black). % It's black's turn.
