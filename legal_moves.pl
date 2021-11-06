:- [operators].
:- [utilities].

% Generate all valid moves from given position for Color
valid_moves(PlayerPieces vs OpponentPieces, Color, LastMove, Moves) :-
	findall(Move, valid_move(PlayerPieces,PlayerPieces vs OpponentPieces, Color, LastMove, Move), Moves).

% Find a valid move from the given position for Color
valid_move([Type@Origin | Rest], Pieces, Color, LastMove, Move) :-
	legal_move(Type@Origin, Pieces, Color, LastMove, Position),
	Move = (Type@Origin goto Position)
	;
	valid_move(Rest, Pieces, Color, LastMove, Move).

% Legal moves for each piece
legal_move(rook@Origin, Pieces, _, _, Position) :-
	straight(Origin, Pieces, Position).
legal_move(bishop@Origin, Pieces, _, _, Position) :-
	diagonal(Origin, Pieces, Position).
legal_move(queen@Origin, Pieces, _, _, Position) :-
	diagonal(Origin, Pieces, Position)
	;
	straight(Origin, Pieces, Position).
legal_move(king@Origin, PlayerPieces vs OpponentPieces, Colour, _, Position) :-
	step(Origin, _, Position),
	valid_square(Position),
	\+ position_taken(Position, PlayerPieces),
	apply_move(PlayerPieces vs OpponentPieces, king@Origin goto Position, NewPieces),
	\+ check(NewPieces, Colour, king@Origin goto Position).
legal_move(knight@Origin, PlayerPieces vs _, _, _, Position) :-
	knight_step(Origin, Position),
	valid_square(Position),
	\+ position_taken(Position, PlayerPieces).
legal_move(pawn@Origin, PlayerPieces vs OpponentPieces, Color, LastMove, Position) :-
	pawn_move(Origin, PlayerPieces vs OpponentPieces, Color, Position);
	pawn_take(Origin, PlayerPieces vs OpponentPieces, Color, LastMove, Position).

% Check check!
check(PlayerPieces vs OpponentPieces, Colour, LastMove) :-
	member(king@Position, PlayerPieces),
	member(Piece@PiecePosition, OpponentPieces),
	Piece \= king,
	switch_colour(Colour, NewColour),
	legal_move(Piece@PiecePosition, OpponentPieces vs PlayerPieces, NewColour, LastMove, Position).

% Go diagonally as legally allowed
diagonal(Origin, Pieces, Position) :-
	move(Origin, up_right, Pieces, Position);
	move(Origin, up_left, Pieces, Position);
	move(Origin, down_right, Pieces, Position);
	move(Origin, down_left, Pieces, Position).

% Go straight as legally allowed
straight(Origin, Pieces, Position) :-
	move(Origin, up, Pieces, Position);
	move(Origin, down, Pieces, Position);
	move(Origin, right, Pieces, Position);
	move(Origin, left, Pieces, Position).

% Define directions
direction(up, 0/1).
direction(down, 0/(-1)).
direction(right, 1/0).
direction(left, (-1)/0).
direction(up_right, 1/1).
direction(up_left, 1/(-1)).
direction(down_right, (-1)/1).
direction(down_left, (-1)/(-1)).

% Define knight moves
knight_step_diff(1/2).
knight_step_diff((-1)/2).
knight_step_diff(1/(-2)).
knight_step_diff((-1)/(-2)).
knight_step_diff(2/1).
knight_step_diff((-2)/1).
knight_step_diff(2/(-1)).
knight_step_diff((-2)/(-1)).

knight_step(X/Y, NewX/NewY) :-
	knight_step_diff(DX/DY),
	NewX is X + DX,
	NewY is Y + DY.

% Pawn rules
attack_direction(white, up).
attack_direction(black, down).
pawn_rank(white, 2).
pawn_rank(black, 7).

% Make step with pawn
pawn_step(X/Y, PlayerPieces vs OpponentPieces, Color, Position) :-
	attack_direction(Color, Direction),
	step(X/Y, Direction, Position),
	valid_square(Position),
	\+ position_taken(Position, PlayerPieces),
	\+ position_taken(Position, OpponentPieces).

% Step or attack with pawn
pawn_move(X/Y, PlayerPieces vs OpponentPieces, Color, Position) :-
	pawn_step(X/Y, PlayerPieces vs OpponentPieces, Color, Position1),
	(Position = Position1
	;
	pawn_rank(Color, Rank), %Could theoretically just have 2 or 7 here, because you can't go forward twice on the opposit direction anyway.
	Y = Rank,
	pawn_step(Position1, PlayerPieces vs OpponentPieces, Color, Position2),
	Position = Position2).

% Potential pawn attack squares
pawn_attack_square(Origin, Color, Position) :-
	attack_direction(Color, Direction),
	step(Origin, Direction, Forward),
	(step(Forward, right, Position)
	;
	step(Forward, left, Position)),
	valid_square(Position).

% Whether pawn can take
pawn_take(Origin, _ vs OpponentPieces, Color, LastMove, Position) :-
	pawn_attack_square(Origin, Color, Position),
	(position_taken(Position, OpponentPieces)
	;
	pawn_en_passant_virtual_position(LastMove, Position)).

% EN PASSANT!!!!!!!!!!!!!!!!
pawn_en_passant_virtual_position(pawn@X/Y goto X/Y2, X/VirtualY) :-
	2 is abs(Y2 - Y),
	VirtualY is ((Y + Y2) / 2).

% Generate some step
step(X/Y, Direction, NewX/NewY) :-
	direction(Direction, DX/DY),
	NewX is X + DX,
	NewY is Y + DY.

% Make one move
move(Origin, Direction, PlayerPieces vs OpponentPieces, Position) :-
	\+ position_taken(Origin, OpponentPieces),
	step(Origin, Direction, Position),
	valid_square(Position),
	\+ position_taken(Position, PlayerPieces).

% Move and keep moving!
move(Origin, Direction, PlayerPieces vs OpponentPieces, Position2) :-
	\+ position_taken(Origin, OpponentPieces),
	step(Origin, Direction, Position1),
	valid_square(Position1),
	\+ position_taken(Position1, PlayerPieces),
	move(Position1, Direction, PlayerPieces vs OpponentPieces, Position2).

% Square within bounds
valid_square(X/Y) :-
	integer(X),
	X >= 1,
	X =< 8,
	integer(Y),
	Y >= 1,
	Y =< 8.

position_taken(Position, Pieces) :-
	member(_@Position, Pieces).