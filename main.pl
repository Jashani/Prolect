% Programmers - Yaakov Rise & Ben Avtabi
% File Name - main.pl (for now)
% Description - Idfk
% Input - Idfk
% Output - Idfk
% Synopsys - Goodness Idfk

:- [alpha_beta].
:- [legal_moves].
:- [board].

play :-
    pieces_full_board(White vs Black),
    get_user_difficulty(Difficulty), !,
    generate_board(White vs Black, Board),
    print_board(Board),
    turn(White vs Black, Difficulty, [nomove], black).

% turn(WhitePieces vs BlackPieces, Difficulty, PreviousMoves, Colour)
% Runs the game iteratively using last-call optimization.
% Each iteration runs a full game turn (two half-turns).

% Bot is white
turn(Pieces, Difficulty, PreviousMoves, white) :-
    difficulty_to_depth(Difficulty, Pieces, Depth),
    write("Depth = "), write(Depth), nl,
    half_turn(Pieces, Depth, white, PreviousMoves, BotMove), !,
    apply_move(Pieces, BotMove, W1 vs B1),
    generate_board(W1 vs B1, Board),
    print_board(Board),

    check_game_end(W1 vs B1, white, BotMove, Outcome, _), !,
    handle_outcome(Outcome, white),

    player_turn(B1 vs W1, BotMove, PlayerMove, black), !,
    apply_move(B1 vs W1, PlayerMove, B2 vs W2),
    generate_board(W2 vs B2, Board2),
    print_board(Board2),

    check_game_end(B2 vs W2, black, PlayerMove, Outcome1, _),
    handle_outcome(Outcome1, black),

    !, turn(W2 vs B2, Difficulty, [PlayerMove, BotMove | PreviousMoves], white).

% Bot is black
turn(Pieces, Difficulty, [LastMove | PreviousMoves], black) :-
    player_turn(Pieces, LastMove, PlayerMove, white), !,
    apply_move(Pieces, PlayerMove, W1 vs B1),
    generate_board(W1 vs B1, Board1),
    print_board(Board1),

    check_game_end(W1 vs B1, white, PlayerMove, Outcome1, _),
    handle_outcome(Outcome1, white),

    difficulty_to_depth(Difficulty, B1 vs W1, Depth),
    write("Depth = "), write(Depth), nl,
    half_turn(B1 vs W1, Depth, black, [PlayerMove, LastMove | PreviousMoves], BotMove), !,
    apply_move(B1 vs W1, BotMove, B2 vs W2),
    generate_board(W2 vs B2, Board2),
    print_board(Board2),

    check_game_end(B2 vs W2, black, BotMove, Outcome2, _), !,
    handle_outcome(Outcome2, black),

    !, turn(W2 vs B2, Difficulty, [BotMove, PlayerMove, LastMove | PreviousMoves], black).

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

difficulty_to_depth(Difficulty, P1 vs P2, Depth) :-
    length(P1, L1), length(P2, L2),
    NumOfPieces is L1 + L2,
    (
        NumOfPieces > 20, !,
        GamePhase = opening
        ;
        (
            NumOfPieces > 6, !,
            GamePhase = midgame
            ;
            GamePhase = endgame
        )
    ),
    difficulty(Difficulty, GamePhase, Depth).

% difficulty(+Difficulty, +GamePhase, -AssosicatedDepth)
% Correlates a diffculty level and game phase to depth of search.
difficulty(easy, opening, 2).
difficulty(easy, midgame, 2).
difficulty(easy, endgame, 2).
difficulty(medium, opening, 2).
difficulty(medium, midgame, 3).
difficulty(medium, endgame, 3).
difficulty(hard, opening, 3).
difficulty(hard, midgame, 3).
difficulty(hard, endgame, 4).

handle_outcome(play_on, _).
handle_outcome(draw, _) :-
    write('Draw!'), nl,
    fail.
handle_outcome(win, Color) :-
    write(Color), write(" won!"), nl,
    fail.
handle_outcome(loss, Color) :-
    switch_colour(Color, NewColor),
    handle_outcome(win, NewColor).

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
player_turn(PlayerPieces vs OpponentPieces, LastMove, Type@Origin goto Position, Colour) :-
    get_user_move(Type@Origin goto Position),
    member(Type@Origin, PlayerPieces),
    legal_move(Type@Origin, PlayerPieces vs OpponentPieces, Colour, LastMove, Position), !.

% get_user_input(-Move)
% Receives input from the user.
% The user will be repeatedly asked to give a valid move or "ff" that signals he resigns.
get_user_move(Type@Origin goto Position) :-
    write('Enter your move:'), nl,
    read(Input),
    (
        Input = (Type@Origin goto Position)
        ;
        Input \= 'ff',
        write('Invalid move, try again'), nl,
        get_user_move(Type@Origin goto Position)
    ).

get_user_difficulty(Difficulty) :-
    write('Choose a difficulty level (easy / medium / hard):'), nl,
    read(Input),
    (
        difficulty(Input, opening, _),
        Difficulty = Input
        ;
        write('Invalid difficulty, try again.'), nl,
        get_user_difficulty(Difficulty)
    ).