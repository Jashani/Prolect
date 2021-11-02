:-op(1000, yfx, goto).
:-op(900, yfx, @).
:-op(800, yfx, vs).

valid_moves(PlayerPieces vs OpponentPieces, Moves) :-
	findall(Move, valid_move(PlayerPieces,PlayerPieces vs OpponentPieces, Move), Moves).

valid_move([], _) :- fail.
valid_move([Type@Origin | Rest], Pieces, Type@Origin goto Position) :-
	legal_move(Type@Origin, Pieces, Type@Position);
	valid_move(Rest, Pieces, _@Position).

legal_move(rook@Origin, Pieces, rook@Position) :-
	straight(Origin, Pieces, Position).
legal_move(bishop@Origin, Pieces, bishop@Position) :-
	diagonal(Origin, Pieces, Position).
legal_move(queen@Origin, Pieces, queen@Position) :-
	diagonal(Origin, Pieces, Position);
	straight(Origin, Pieces, Position).
legal_move(king@Origin, PlayerPieces vs _, king@Position) :-
	step(Origin, _, Position),
	valid_square(Position),
	\+ position_taken(Position, PlayerPieces).
legal_move(knight@Origin, PlayerPieces vs _, knight@Position) :-
	knight_step(Origin, Position),
	valid_square(Position),
	\+ position_taken(Position, PlayerPieces).


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