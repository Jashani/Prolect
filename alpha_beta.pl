:- [heuristics].
:- [legal_moves].
:- [utilities].
:- [operators].

switch_colour(white, black).
switch_colour(black, white).

% Alphabeta algorithm as presented in the book, along with some naming improvements,
% as well as memory access in bound_best to avoid unnecessary calculations.

% alphabeta(+Depth, +Pieces, +Colour, +PreviousMoves, +Alpha, +Beta, -GoodMove, -Score).
alphabeta(Depth, Pieces, Colour, [LastMove | Rest], Alpha, Beta, GoodMove, Score) :-
    %format('alphabeta:~n - Depth: ~w~n - Pieces: ~w~n - Colour: ~w~n - PreviousMoves: ~w~n - Alpha: ~w~n - Beta: ~w~n - GoodMove: ~w~n - Score: ~w~n', [Depth, Pieces, Colour, [LastMove | Rest], Alpha, Beta, GoodMove, Score]), nl,
    Depth > 0,
    valid_moves(Pieces, Colour, LastMove, Moves), !,
    bound_best(Depth, Pieces, Colour, [LastMove | Rest], Moves, Alpha, Beta, GoodMove, Score)
    ;
    score(Pieces, Colour, Score). % The static value of the position

% bound_best(+Depth, +Pieces, +Colour, +PreviousMoves, +Moves, +Alpha, +Beta, -GoodMove, -GoodScore)
bound_best(Depth, Pieces, Colour, PreviousMoves, [Move | Moves], Alpha, Beta, GoodMove, GoodScore) :-
    %format('bound_best:~n - Depth: ~w~n - Pieces: ~w~n - Colour: ~w~n - PreviousMoves: ~w~n - Moves: ~w~n - Alpha: ~w~n - Beta: ~w~n - GoodMove: ~w~n - GoodScore: ~w~n', [Depth, Pieces, Colour, PreviousMoves, [Move | Moves], Alpha, Beta, GoodMove, GoodScore]), nl,
    apply_move(Pieces, Move, NewPlayer vs NewOpponent),
    check_game_end(NewPlayer vs NewOpponent, Colour, Outcome, EndScore),
    (
        Outcome = play_on, !,
        write("Looking into "), write(Move), nl,
        switch_colour(Colour, NewColour),
        NewDepth is Depth - 1,
        alphabeta(NewDepth, NewOpponent vs NewPlayer, NewColour, [Move | PreviousMoves], Alpha, Beta, _, Score),
        write("After alphabeta for move"), write(Move), nl
        ;
        write("Found end"),
        Score = EndScore
    ),
    %format('good_enough:~n - Depth: ~w~n - Pieces: ~w~n - Colour: ~w~n - PreviousMoves: ~w~n - Moves: ~w~n - Alpha: ~w~n - Beta: ~w~n - Move: ~w~n - Score: ~w~n - GoodMove: ~w~n - GoodScore: ~w~n', [Depth, Pieces, Colour, PreviousMoves, Moves, Alpha, Beta, Move, Score, GoodMove, GoodScore]), nl,
    good_enough(Depth, Pieces, Colour, PreviousMoves, Moves, Alpha, Beta, Move, Score, GoodMove, GoodScore).

% good_enough(+Depth, +Pieces, +Colour, +PreviousMoves, +Moves, +Alpha, +Beta, +Move, +Score, -GoodMove, -GoodScore)
good_enough(_, _, _, _, [], _, _, Move, Score, Move, Score) :- !. % No other candidate

good_enough(_, _, Colour, _, _, Alpha, Beta, Move, Score, Move, Score) :-
    min_to_move(Colour), Score > Beta, ! % Maximizer attained upper bound
    ;
    max_to_move(Colour), Score < Alpha, !. % Minimizer attained lower bound

good_enough(Depth, Pieces, Colour, PreviousMoves, Moves, Alpha, Beta, Move, Score, GoodMove, GoodScore) :-
    switch_colour(Colour, NewColour),
    new_bounds(Alpha, Beta, NewColour, Score, NewAlpha, NewBeta), % Refine bounds
    bound_best(Depth, Pieces, Colour, PreviousMoves, Moves, NewAlpha, NewBeta, CandidateMove, CandidateScore),
    better_of(Move, NewColour, Score, CandidateMove, CandidateScore, GoodMove, GoodScore).

new_bounds(Alpha, Beta, Colour, Score, Score, Beta) :-
    min_to_move(Colour), Score > Alpha, !. % Maximizer increased lower bound

new_bounds(Alpha, Beta, Colour, Score, Alpha, Score) :-
    max_to_move(Colour), Score < Beta, !. % Minimizer decreased upper bound

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
