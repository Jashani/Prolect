:-op(1000, yfx, goto).
:-op(900, yfx, @).
:-op(800, yfx, vs).

valid_moves(PlayerPieces vs OpponentPieces, Color, LastMove, Moves) :-
	findall(Move, valid_move(PlayerPieces,PlayerPieces vs OpponentPieces, Color, LastMove, Move), Moves).

valid_move([Type@Origin | Rest], Pieces, Color, LastMove, Move) :-
	(legal_move(Type@Origin, Pieces, Color, LastMove, Position),
	Move = (Type@Origin goto Position)
	;
	valid_move(Rest, Pieces, Color, LastMove, Move)).

legal_move(rook@Origin, Pieces, _, _, Position) :-
	straight(Origin, Pieces, Position).
legal_move(bishop@Origin, Pieces, _, _, Position) :-
	diagonal(Origin, Pieces, Position).
legal_move(queen@Origin, Pieces, _, _, Position) :-
	diagonal(Origin, Pieces, Position);
	straight(Origin, Pieces, Position).
legal_move(king@Origin, PlayerPieces vs _, _, _, Position) :-
	step(Origin, _, Position),
	valid_square(Position),
	\+ position_taken(Position, PlayerPieces).
legal_move(knight@Origin, PlayerPieces vs _, _, _, Position) :-
	knight_step(Origin, Position),
	valid_square(Position),
	\+ position_taken(Position, PlayerPieces).
legal_move(pawn@Origin, PlayerPieces vs OpponentPieces, Color, LastMove, Position) :-
	pawn_move(Origin, PlayerPieces vs OpponentPieces, Color, Position);
	pawn_take(Origin, PlayerPieces vs OpponentPieces, Color, LastMove, Position).

diagonal(Origin, Pieces, Position) :-
	move(Origin, up_right, Pieces, Position);
	move(Origin, up_left, Pieces, Position);
	move(Origin, down_right, Pieces, Position);
	move(Origin, down_left, Pieces, Position).

straight(Origin, Pieces, Position) :-
	move(Origin, up, Pieces, Position);
	move(Origin, down, Pieces, Position);
	move(Origin, right, Pieces, Position);
	move(Origin, left, Pieces, Position).

direction(up, 0/1).
direction(down, 0/(-1)).
direction(right, 1/0).
direction(left, (-1)/0).
direction(up_right, 1/1).
direction(up_left, 1/(-1)).
direction(down_right, (-1)/1).
direction(down_left, (-1)/(-1)).

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

attack_direction(white, up).
attack_direction(black, down).
pawn_rank(white, 2).
pawn_rank(black, 7).

pawn_step(X/Y, PlayerPieces vs OpponentPieces, Color, Position) :-
	attack_direction(Color, Direction),
	step(X/Y, Direction, Position),
	valid_square(Position),
	\+ position_taken(Position, PlayerPieces),
	\+ position_taken(Position, OpponentPieces).

pawn_move(X/Y, PlayerPieces vs OpponentPieces, Color, Position) :-
	pawn_step(X/Y, PlayerPieces vs OpponentPieces, Color, Position1),
	(Position = Position1
	;
	pawn_rank(Color, Rank), %Could theoretically just have 2 or 7 here, because you can't go forward twice on the opposit direction anyway.
	Y = Rank,
	pawn_step(Position1, PlayerPieces vs OpponentPieces, Color, Position2),
	Position = Position2).

pawn_attack_square(Origin, Color, Position) :-
	attack_direction(Color, Direction),
	step(Origin, Direction, Forward),
	(step(Forward, right, Position)
	;
	step(Forward, left, Position)),
	valid_square(Position).

pawn_take(Origin, _ vs OpponentPieces, Color, LastMove, Position) :-
	pawn_attack_square(Origin, Color, Position),
	(position_taken(Position, OpponentPieces)
	;
	pawn_en_passant_virtual_position(LastMove, Position)).

pawn_en_passant_virtual_position(pawn@X/Y goto X/Y2, X/VirtualY) :-
	2 is abs(Y2 - Y),
	VirtualY is ((Y + Y2) / 2).

step(X/Y, Direction, NewX/NewY) :-
	direction(Direction, DX/DY),
	NewX is X + DX,
	NewY is Y + DY.

move(Origin, Direction, PlayerPieces vs OpponentPieces, Position) :-
	\+ position_taken(Origin, OpponentPieces),
	step(Origin, Direction, Position),
	valid_square(Position),
	\+ position_taken(Position, PlayerPieces).

move(Origin, Direction, PlayerPieces vs OpponentPieces, Position2) :-
	\+ position_taken(Origin, OpponentPieces),
	step(Origin, Direction, Position1),
	valid_square(Position1),
	\+ position_taken(Position1, PlayerPieces),
	move(Position1, Direction, PlayerPieces vs OpponentPieces, Position2).

valid_square(X/Y) :-
	integer(X),
	X >= 1,
	X =< 8,
	integer(Y),
	Y >= 1,
	Y =< 8.

position_taken(Position, Pieces) :-
	member(_@Position, Pieces).