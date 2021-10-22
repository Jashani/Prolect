% Alphabeta algorithm as presented in the book, along with some naming improvements,
% as well as memory access in boundedbest to avoid unnecessary calculations.

alphabeta(Position, Alpha, Beta, GoodPosition, Val) :-
    moves(Position, Positions), !,
    boundedbest(Positions, Alpha, Beta, GoodPosition, Val);
    staticval(Position, Val). % Static value of Position

boundedbest([Position|Positions], Alpha, Beta, GoodPosition, GoodVal) :-
    (retract(value(Position, Val)), !, % Value already been calculated, no need to recalculate.
     assert(value(Position, Val))
     ;
     alphabeta(Position, Alpha, Beta, _, Val), 
     assert(value(Position, Val))),
    goodenough(Positions, Alpha, Beta, Position, Val, GoodPosition, GoodVal).

goodenough([], _, _, Position, Val, Position, Val) :- !. % No other candidate

goodenough(_, Alpha, Beta, Position, Val, Position, Val) :-
    min_to_move(Position), Val > Beta, ! % Maximizer attained upper bound
    ;
    max_to_move(Position), Val < Alpha, !. % Minimizer attained lower bound

goodenough(Positions, Alpha, Beta, Position, Val, GoodPosition, GoodVal) :-
    newbounds(Alpha, Beta, Position, Val, NewAlpha, NewBeta), % Refine bounds
    boundedbest(Positions, NewAlpha, NewBeta, Position1, Val1),
    betterof(Position, Val, Position1, Val1, GoodPosition, GoodVal).

newbounds(Alpha, Beta, Position, Val, Val, Beta) :-
    min_to_move(Position), Val > Alpha, !. % Maximizer increased lower bound

newbounds(Alpha, Beta, Position, Val, Alpha, Val) :-
    max_to_move(Position), Val < Beta, !. % Minimizer decreased upper bound

newbounds(Alpha, Beta, _, _, Alpha, Beta). % Otherwise bounds unchanged

betterof(Position, Val, _, Val1, Position, Val) :- % Position better than Position1
    min_to_move(Position), Val > Val1, !
    ;
    max_to_move(Position), Val < Val1, !.

betterof(_, _, Position1, Val1, Position1, Val1).  % Otherwise Position1 better

% updateRow(Player, Row, NewRow)
% Place a player in an empty cell

updateRow(Player, [?, A, B], [Player, A, B]).
updateRow(Player, [A, ?, B], [A, Player, B]).
updateRow(Player, [A, B, ?], [A, B, Player]).

% opponentPlayer(Player, Opponent)
% The opponent is whatever the player isn't

opponentPlayer(x, o).
opponentPlayer(o, x).

max_to_move(position(x, _)). % It's x's turn.

min_to_move(position(o, _)). % It's o's turn.

% move(CurrentPosition, PotentialPosition)
% Attempt to place opponent in each potential location in current position.

move(position(Player, [Row1, Row2, Row3]), position(OtherPlayer, [NewRow1, Row2, Row3])) :-
    opponentPlayer(Player, OtherPlayer),
    updateRow(Player, Row1, NewRow1).

move(position(Player, [Row1, Row2, Row3]), position(OtherPlayer, [Row1, NewRow2, Row3])) :-
    opponentPlayer(Player, OtherPlayer),
    updateRow(Player, Row2, NewRow2).

move(position(Player, [Row1, Row2, Row3]), position(OtherPlayer, [Row1, Row2, NewRow3])) :-
    opponentPlayer(Player, OtherPlayer),
    updateRow(Player, Row3, NewRow3).

moves(position(_, Board), _) :- % Case where the board is full, therefore we have no moves.
    % It isn't necessary to check for loss/victory here, as we do that after every move.
    findall(X, (X = x, member(Row, Board), member(X, Row)), MaxMarks), 
    findall(X, (X = o, member(Row, Board), member(X, Row)), MinMarks),
    length(MaxMarks, MaxMarkCount),
    length(MinMarks, MinMarkCount), 
    (MaxMarkCount + MinMarkCount) =:= 9, !,
    fail.

moves(Position, Positions) :- % Find possible moves for current player.
    (max_to_move(Position), !,
     moves(Position, [], Positions))
    ;
    (min_to_move(Position),
     moves(Position, [], Positions)).

moves(Position, ExistingPositions, NewPositions) :-
    move(Position, NewPosition), % Generate some move.
    not(member(NewPosition, ExistingPositions)), !, % Skip if already found.
    moves(Position, [NewPosition | ExistingPositions], NewPositions). % Generate more moves.

moves(_, Positions, Positions). % Exhausted all possibilities.

winner(Board, Player) :- % A victory position is reached.
    (member([Player, Player, Player], Board) % Victory by row.
    ;
    (Board = [[Player, _, _], [_, Player, _], [_, _, Player]]) % Victory by diagonal.
    ;
    (Board = [[_, _, Player], [_, Player, _], [Player, _, _]]) % Victory by diagonal.
    ;
    (Board = [[Player, _, _], [Player, _, _], [Player, _, _]]) % Victory by column.
    ;
    (Board = [[_, Player, _], [_, Player, _], [_, Player, _]]) % Victory by column.
    ;
    (Board = [[_, _, Player], [_, _, Player], [_, _, Player]])), % Victory by column.
    (Player == x; Player == o). % Ensure proper instantiation.

staticval(position(_, Board), Val) :- % 1 for x victory, -1 for o victory, 0 for draw.
    winner(Board, Player),
    ((Player == x, !, Val = 1)
    ;
    (Player == o, !, Val = -1))
    ;   
    Val = 0.