% Programmers - Yaakov Rise & Ben Avtabi
% File Name - main.pl (for now)
% Description - Idfk
% Input - Idfk
% Output - Idfk
% Synopsys - Goodness Idfk

:- [alpha_beta].
:- [legal_moves].

half_turn(PlayerPieces, OpponentPieces, Depth, NewPlayerPieces, NewOpponentPieces) :-
    valid_moves(PlayerPieces, Moves), % Eventually need to pass OpponentPieces as well
    best_move(Moves, Depth, Move),
    apply_move(PlayerPieces, OpponentPieces, Move, NewPlayerPieces, NewOpponentPieces).

% Make appropriate changes in player piece lists according to move.
apply_move(PlayerPieces, OpponentPieces, Type@Origin goto Position, NewPlayerPieces, NewOpponentPieces) :-
    remove_piece(Type@Origin, PlayerPieces, TempPlayerPieces),
    remove_piece(_@Position, OpponentPieces, NewOpponentPieces),
    insert_piece(Type@Position, TempPlayerPieces, NewPlayerPieces), !.

% Insert a piece into its appropriate place in a list of ordered pieces.
insert_piece(Piece, [], [Piece]).
insert_piece(P1@X1/Y1, [P2@X2/Y2 | Rest], Pieces) :-
    X1 < X2,
    Y1 >= Y2,
    Pieces = [P1@X1/Y1, P2@X2/Y2 | Rest], !
    ;
    insert_piece(P1@X1/Y1, Rest, TempPieces),
    Pieces = [P2@X2/Y2 | TempPieces].

  % Remove a piece from a list, if the piece exists.
remove_piece(_, [], []).
remove_piece(Piece, Pieces, NewPieces) :-
    delete(Pieces, Piece, NewPieces), !;
    NewPieces = Pieces.

best_move([Move | _], _, Move).

play :-
    Black = [rook@1/8, knight@2/8, bishop@3/8, queen@4/8, king@5/8, bishop@6/8, knight@7/8, rook@8/8,
            pawn@1/7, pawn@2/7, pawn@3/7, pawn@4/7, pawn@5/7, pawn@6/7, pawn@7/7, pawn@8/7],
    White = [pawn@1/2, pawn@2/2, pawn@3/2, pawn@4/2, pawn@5/2, pawn@6/2, pawn@7/2, pawn@8/2,
            rook@1/1, knight@2/1, bishop@3/1, queen@4/1, king@5/1, bishop@6/1, knight@7/1, rook@8/1],
    half_turn(White, Black, 0, NewWhite, NewBlack),
    half_turn(NewBlack, NewWhite, 0, NewNewBlack, NewNewWhite).

pieces(P1, P2) :-  % For testing
    P1 = [pawn@1/8, pawn@6/8, pawn@7/8],
    P2 = [pawn@1/7, pawn@4/8, pawn@3/7].