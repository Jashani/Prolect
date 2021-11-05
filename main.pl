% Programmers - Yaakov Rise & Ben Avtabi
% File Name - main.pl (for now)
% Description - Idfk
% Input - Idfk
% Output - Idfk
% Synopsys - Goodness Idfk

:- [alpha_beta].
:- [legal_moves].
:- [board].

half_turn(Pieces, Depth, Color, [LastMove | Rest], BestMove) :-
    valid_moves(Pieces, Color, LastMove, ValidMoves),
    write("Possible moves: "), write(ValidMoves), nl,
    best_move(ValidMoves, Depth, Color, [LastMove | Rest], BestMove).

% is_en_passant(+OpponentPieces, +Move, -VirtualPawnPosition)
% Checks if Move describes a pawn taking another pawn through "en passant",
% and returns the position of the taken pawn.
% Assumes Move is a valid move.
is_en_passant(OpponentPieces, pawn@X1/Y1 goto X2/Y2, X2/Y1) :-
    X1 =\= X2,
    \+ position_taken(X2/Y2, OpponentPieces).

% apply_move(+(PlayerPieces vs OpponentPieces), +Move, -(NewPlayerPieces vs NewOpponentPieces))
% Make appropriate changes in player piece lists according to move.
apply_move(PlayerPieces vs OpponentPieces, pawn@Origin goto Position, NewPlayerPieces vs NewOpponentPieces) :-
    is_en_passant(OpponentPieces, pawn@Origin goto Position, PawnRealPosition), !,
    remove_piece(pawn@Origin, PlayerPieces, TempPlayerPieces),
    insert_piece(pawn@Position, TempPlayerPieces, NewPlayerPieces),
    remove_piece(pawn@PawnRealPosition, OpponentPieces, NewOpponentPieces), !.

apply_move(PlayerPieces vs OpponentPieces, Type@Origin goto Position, NewPlayerPieces vs NewOpponentPieces) :-
    remove_piece(Type@Origin, PlayerPieces, TempPlayerPieces),
    insert_piece(Type@Position, TempPlayerPieces, NewPlayerPieces),
    remove_piece(_@Position, OpponentPieces, NewOpponentPieces), !.

% insert_piece(+Piece, +Pieces, -NewPieces)
% Insert a piece into its appropriate place in a list of ordered pieces.
insert_piece(Piece, [], [Piece]).
insert_piece(P1@X1/Y1, [P2@X2/Y2 | Rest], Pieces) :-
    X1 < X2,
    Y1 >= Y2,
    Pieces = [P1@X1/Y1, P2@X2/Y2 | Rest], !
    ;
    insert_piece(P1@X1/Y1, Rest, TempPieces),
    Pieces = [P2@X2/Y2 | TempPieces].

% remove_piece(+Piece, +Pieces, -NewPieces)
% Remove a piece from a list, if the piece exists.
remove_piece(_, [], []).
remove_piece(Piece, Pieces, NewPieces) :-
    delete(Pieces, Piece, NewPieces), !;
    NewPieces = Pieces.

% best_move(+Moves, +Depth, +PlayingColor, +PreviousMoves, -BestMove)
best_move([Move | _], _, _, _, Move).

play :-
    Black = [rook@1/8, knight@2/8, bishop@3/8, queen@4/8, king@5/8, bishop@6/8, knight@7/8, rook@8/8,
            pawn@1/7, pawn@2/7, pawn@3/7, pawn@4/7, pawn@5/7, pawn@6/7, pawn@7/7, pawn@8/7],
    White = [pawn@1/2, pawn@2/2, pawn@3/2, pawn@4/2, pawn@5/2, pawn@6/2, pawn@7/2, pawn@8/2,
            rook@1/1, knight@2/1, bishop@3/1, queen@4/1, king@5/1, bishop@6/1, knight@7/1, rook@8/1],
    %White = [rook@1/8, bishop@6/8], %Swap that in when debugging
    %Black = [rook@1/7, bishop@4/8],
    turn(White vs Black, easy, [nomove]).

% difficulty(Difficulty, AssosicatedDepth)
% Correlates a diffculty level to depth of search.
difficulty(easy, 1).
difficulty(medium, 2).
difficulty(hard, 3).

% turn(WhitePieces vs BlackPieces, Difficulty, PreviousMoves)
% Runs the game iteratively using last-call optimization.
% Each iteration runs an full game turn (2 half-turns).
turn(Pieces, Difficulty, PreviousMoves) :-
    difficulty(Difficulty, Depth),
    half_turn(Pieces, Depth, white, PreviousMoves, BotMove),
    apply_move(Pieces, BotMove, W1 vs B1),
    write(BotMove),nl,
    %generate_board(W1 vs B1, Board),    % Doesn't work after a few turns (probably because it's not sorted)
    %print_board(Board),

    player_turn(B1 vs W1, BotMove, PlayerMove),
    apply_move(B1 vs W1, PlayerMove, B2 vs W2),
    %generate_board(W2 vs B2, Board2),
    %print_board(Board2),

    !, turn(W2 vs B2, Difficulty, [PlayerMove, BotMove | PreviousMoves]).

pieces(P1, P2) :-  % For testing
    P1 = [pawn@1/8, pawn@6/8, pawn@7/8],
    P2 = [pawn@1/7, pawn@4/8, pawn@3/7].

pieces_no_pawns(P1 vs P2) :-  % For testing
    P1 = [rook@1/8, bishop@6/8],
    P2 = [rook@1/7, bishop@4/8].

pieces_full_board(White vs Black) :-
    White = [pawn@1/2, pawn@2/2, pawn@3/2, pawn@4/2, pawn@5/2, pawn@6/2, pawn@7/2, pawn@8/2,
            rook@1/1, knight@2/1, bishop@3/1, queen@4/1, king@5/1, bishop@6/1, knight@7/1, rook@8/1],
    Black = [rook@1/8, knight@2/8, bishop@3/8, queen@4/8, king@5/8, bishop@6/8, knight@7/8, rook@8/8,
            pawn@1/7, pawn@2/7, pawn@3/7, pawn@4/7, pawn@5/7, pawn@6/7, pawn@7/7, pawn@8/7].
    
% player_turn(+(PlayerPieces vs OpponentPieces), +LastMove, -Move)
% Gets a move from the player.
player_turn(PlayerPieces vs OpponentPieces, LastMove, Type@Origin goto Position) :-
    get_user_input(Type@Origin goto Position),
    member(Type@Origin, PlayerPieces),
    legal_move(Type@Origin, PlayerPieces vs OpponentPieces, black, LastMove, Position).

% get_user_input(-Move)
% Receives input from the user.
% The user will be repeatedly asked to give a valid move or "ff" that signals he resigns.
get_user_input(Type@Origin goto Position) :-
    read(Input),
    ((Input = (Type@Origin goto Position))
        ;
    (Input \= 'ff',
    write("Invalid move, try again"),
    get_user_input(Type@Origin goto Position))).
