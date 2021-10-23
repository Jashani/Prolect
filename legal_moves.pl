valid_moves(P1, Moves) :-
	findall(Move, valid_move(P1, Move), Moves).

valid_move([], _) :- fail.
valid_move([First | Rest], Move) :-
	legal_move(First, Move);
	valid_move(Rest, Move).

legal_move(rook/X/Y, rook/Position) :-
	straight(X/Y, Position).
legal_move(bishop/X/Y, bishop/Position) :-
	diagonal(X/Y, Position).
legal_move(queen/X/Y, queen/Position) :-
	diagonal(X/Y, Position);
	straight(X/Y, Position).
legal_move(king/X/Y, king/Position) :-
	(step_up(X/Y, Position);step_down(X/Y, Position);step_right(X/Y, Position);step_left(X/Y, Position);
	step_up_right(X/Y, Position);step_up_left(X/Y, Position);step_down_right(X/Y, Position);step_down_left(X/Y, Position)),
	valid_square(Position).

diagonal(Origin, Position) :-
	up_right(Origin, Position);
	up_left(Origin, Position);
	down_right(Origin, Position);
	down_left(Origin, Position).

straight(Origin, Position) :-
	up(Origin, Position);
	down(Origin, Position);
	right(Origin, Position);
	left(Origin, Position).

step_up_right(X/Y, X3/Y3) :-
	step_up(X/Y, X2/Y2),
	step_right(X2/Y2, X3/Y3).
up_right(X/Y, _) :- (Y >= 8; X >= 8), !, fail.
up_right(X/Y, X2/Y2) :-
	step_up_right(X/Y, X2/Y2).
up_right(X/Y, X3/Y3) :-
	step_up_right(X/Y, X2/Y2),
	up_right(X2/Y2, X3/Y3).

step_up_left(X/Y, X3/Y3) :-
	step_up(X/Y, X2/Y2),
	step_left(X2/Y2, X3/Y3).
up_left(X/Y, _) :- (Y >= 8; X =< 1), !, fail.
up_left(X/Y, X2/Y2) :-
	step_up_left(X/Y, X2/Y2).
up_left(X/Y, X3/Y3) :-
	step_up_left(X/Y, X2/Y2),
	up_left(X2/Y2, X3/Y3).

step_down_right(X/Y, X3/Y3) :-
	step_down(X/Y, X2/Y2),
	step_right(X2/Y2, X3/Y3).
down_right(X/Y, _) :- (Y =< 1; X >= 8), !, fail.
down_right(X/Y, X2/Y2) :-
	step_down_right(X/Y, X2/Y2).
down_right(X/Y, X3/Y3) :-
	step_down_right(X/Y, X2/Y2),
	down_right(X2/Y2, X3/Y3).

step_down_left(X/Y, X3/Y3) :-
	step_down(X/Y, X2/Y2),
	step_right(X2/Y2, X3/Y3).
down_left(X/Y, _) :- (Y =< 1; X =< 1), !, fail.
down_left(X/Y, X2/Y2) :-
	step_down_left(X/Y, X2/Y2).
down_left(X/Y, X3/Y3) :-
	step_down_left(X/Y, X2/Y2),
	down_left(X2/Y2, X3/Y3).

step_up(X/Y, X/Y2) :- Y2 is Y+1.
up(_/Y, _) :- Y >= 8, !, fail.
up(X/Y, X/Y2) :-
	step_up(X/Y, X/Y2).
up(X/Y, X/Y3) :-
	step_up(X/Y, X/Y2),
	up(X/Y2, X/Y3).

step_down(X/Y, X/Y2) :- Y2 is Y-1.
down(_/Y, _) :- Y =< 1, !, fail.
down(X/Y, X/Y2) :-
	step_down(X/Y, X/Y2).
down(X/Y, X/Y3) :-
	step_down(X/Y, X/Y2),
	down(X/Y2, X/Y3).

step_right(X/Y, X2/Y) :- X2 is X+1.
right(X/_, _) :- X >= 8, !, fail.
right(X/Y, X2/Y) :-
	step_right(X/Y, X2/Y).
right(X/Y, X3/Y) :-
	step_right(X/Y, X2/Y),
	right(X2/Y, X3/Y).

step_left(X/Y, X2/Y) :- X2 is X-1.
left(X/_, _) :- X =< 1, !, fail.
left(X/Y, X2/Y) :-
	step_left(X/Y, X2/Y).
left(X/Y, X3/Y) :-
	step_left(X/Y, X2/Y),
	left(X2/Y, X3/Y).

valid_square(X/Y) :-
	integer(X),
	X >= 1,
	X =< 8,
	integer(Y),
	Y >= 1,
	Y =< 8.