valid_moves(P1, Moves) :-
	findall(Move, valid_move(P1, Move), Moves).

valid_move([], _) :- fail.
valid_move([First | Rest], Move) :-
	legal_move(First, Move);
	valid_move(Rest, Move).

legal_move(rook/Origin, rook/Position) :-
	straight(X/Y, Position).
legal_move(bishop/Origin, bishop/Position) :-
	diagonal(Origin, Position).
legal_move(queen/Origin, queen/Position) :-
	diagonal(Origin, Position);
	straight(Origin, Position).
legal_move(king/Origin, king/Position) :-
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

move(X/Y, Direction, NewX/NewY) :-
	step(X/Y, Direction, NewX/NewY),
	valid_square(NewX/NewY).

move(X/Y, Direction, NewX2/NewY2) :-
	step(X/Y, Direction, NewX1/NewY1),
	valid_square(NewX1/NewY1),
	move(NewX1/NewY1, Direction, NewX2/NewY2).

valid_square(X/Y) :-
	integer(X),
	X >= 1,
	X =< 8,
	integer(Y),
	Y >= 1,
	Y =< 8.