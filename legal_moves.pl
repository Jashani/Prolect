:-op(1000, yfx, goto).
:-op(900, yfx, @).

valid_moves(Pieces, Moves) :-
	findall(Move, valid_move(Pieces, Move), Moves).

valid_move([], _) :- fail.
valid_move([Type@Origin | Rest], Type@Origin goto Position) :-
	legal_move(Type@Origin, Type@Position);
	valid_move(Rest, _@Position).

legal_move(rook@Origin, rook@Position) :-
	straight(Origin, Position).
legal_move(bishop@Origin, bishop@Position) :-
	diagonal(Origin, Position).
legal_move(queen@Origin, queen@Position) :-
	diagonal(Origin, Position);
	straight(Origin, Position).
legal_move(king@Origin, king@Position) :-
	step(Origin, _, Position),
	valid_square(Position).

diagonal(Origin, Position) :-
	move(Origin, up_right, Position);
	move(Origin, up_left, Position);
	move(Origin, down_right, Position);
	move(Origin, down_left, Position).

straight(Origin, Position) :-
	move(Origin, up, Position);
	move(Origin, down, Position);
	move(Origin, right, Position);
	move(Origin, left, Position).

direction(up, 0/1).
direction(down, 0/(-1)).
direction(right, 1/0).
direction(left, (-1)/0).
direction(up_right, 1/1).
direction(up_left, 1/(-1)).
direction(down_right, (-1)/1).
direction(down_left, (-1)/(-1)).

step(X/Y, Direction, NewX/NewY) :-
	direction(Direction, DX/DY),
	NewX is X + DX,
	NewY is Y + DY.

move(Origin, Direction, Position) :-
	step(Origin, Direction, Position),
	valid_square(Position).

move(Origin, Direction, Position2) :-
	step(Origin, Direction, Position1),
	valid_square(Position1),
	move(Position1, Direction, Position2).

valid_square(X/Y) :-
	integer(X),
	X >= 1,
	X =< 8,
	integer(Y),
	Y >= 1,
	Y =< 8.