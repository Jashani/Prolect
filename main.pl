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
    alphabeta(Depth, Pieces, Color, [LastMove | Rest], -10000, 10000, BestMove, Score),
    format('Best move is: ~w (~w)', [BestMove, Score]), nl.

% insert_piece(+Piece, +Pieces, -NewPieces)
% Insert a piece to list.
insert_piece(Piece, Pieces, [Piece | Pieces]).

% remove_piece(+Piece, +Pieces, -NewPieces)
% Remove a piece from a list, if the piece exists.
remove_piece(_, [], []).
remove_piece(Piece, Pieces, NewPieces) :-
    delete(Pieces, Piece, NewPieces), !
    ;
    NewPieces = Pieces.

% best_move(+Moves, +Depth, +PlayingColor, +PreviousMoves, -BestMove)
best_move([Move | _], _, _, _, Move).

play :-
    Black = [queen@4/8],
    White = [pawn@4/4, king@1/1],
    %Black = [rook@1/8, knight@2/8, bishop@3/8, queen@4/8, king@5/8, bishop@6/8, knight@7/8, rook@8/8,
    %        pawn@1/7, pawn@2/7, pawn@3/7, pawn@4/7, pawn@5/7, pawn@6/7, pawn@7/7, pawn@8/7],
    %White = [pawn@1/2, pawn@2/2, pawn@3/2, pawn@4/2, pawn@5/2, pawn@6/2, pawn@7/2, pawn@8/2,
    %        rook@1/1, knight@2/1, bishop@3/1, queen@4/1, king@5/1, bishop@6/1, knight@7/1, rook@8/1],
    %White = [rook@1/8, bishop@6/8], %Swap that in when debugging
    %Black = [rook@1/7, bishop@4/8],
    get_user_difficulty(Difficulty), !,
    turn(White vs Black, Difficulty, [nomove]).

difficulty_to_depth(Difficulty, P1 vs P2, Depth) :-
    length(P1, L1), length(P2, L2),
    NumOfPieces is L1 + L2,
    (
        NumOfPieces > 25, !,
        GamePhase = opening
        ;
        (
            NumOfPieces > 10, !,
            GamePhase = midgame
            ;
            GamePhase = endgame
        )
    ),
    difficulty(Difficulty, GamePhase, Depth).

% difficulty(+Difficulty, +GamePhase, -AssosicatedDepth)
% Correlates a diffculty level and game phase to depth of search.
difficulty(easy, opening, 3).
difficulty(easy, midgame, 4).
difficulty(easy, endgame, 4).
difficulty(medium, opening, 4).
difficulty(medium, midgame, 5).
difficulty(medium, endgame, 6).
difficulty(hard, opening, 6).
difficulty(hard, midgame, 8).
difficulty(hard, endgame, 10).
difficulty(hell, opening, 8).
difficulty(hell, midgame, 10).
difficulty(hell, endgame, 14).

handle_outcome(play_on, _).
handle_outcome(draw, _) :-
    write("It's a draw!"), nl,
    fail.
handle_outcome(win, Color) :-
    write(Color), write(" won!"), nl,
    fail.

% turn(WhitePieces vs BlackPieces, Difficulty, PreviousMoves)
% Runs the game iteratively using last-call optimization.
% Each iteration runs an full game turn (2 half-turns).
turn(Pieces, Difficulty, PreviousMoves) :-
    difficulty_to_depth(Difficulty, Pieces, Depth),
    write("Depth = "), write(Depth), nl,
    half_turn(Pieces, Depth, white, PreviousMoves, BotMove), !,
    apply_move(Pieces, BotMove, W1 vs B1),
    generate_board(W1 vs B1, Board),    % Doesn't work after a few turns (probably because it's not sorted)
    print_board(Board),

    check_game_end(W1 vs B1, white, Outcome, _),
    handle_outcome(Outcome, white),

    player_turn(B1 vs W1, BotMove, PlayerMove),
    apply_move(B1 vs W1, PlayerMove, B2 vs W2),
    generate_board(W2 vs B2, Board2),
    print_board(Board2),

    check_game_end(B2 vs W2, black, Outcome1, _),
    handle_outcome(Outcome1, black),

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
    get_user_move(Type@Origin goto Position),
    member(Type@Origin, PlayerPieces),
    legal_move(Type@Origin, PlayerPieces vs OpponentPieces, black, LastMove, Position), !.

% get_user_input(-Move)
% Receives input from the user.
% The user will be repeatedly asked to give a valid move or "ff" that signals he resigns.
get_user_move(Type@Origin goto Position) :-
    write('Enter your move:'), nl,
    read(Input),
    (
        Input = (Type@Origin goto Position)
        ;
        (
            Input \= 'ff',
            write('Invalid move, try again'), nl,
            get_user_move(Type@Origin goto Position)
        )
    ).

get_user_difficulty(Difficulty) :-
    write('Choose a difficulty level (easy/medium/hard/hell):'), nl,
    read(Input),
    (
        (
            difficulty(Input, opening, _),
            Difficulty = Input
        )
        ;
        (
            write('Invalid difficulty, try again.'), nl,
            get_user_difficulty(Difficulty)
        )
    ).