% Programmers - Yaakov Rise & Ben Avtabi
% File Name - main.pl (for now)
% Description - Play a game of chess against a computer.
% Synopsys - Enter `play.` to start a game. 
% Follow the instructions as they show up on the screen.
% The rules match regular chess rules; Castling is unimplemented.
% The move format is: piece@X/Y goto NewX/NewY.
% You may enter `ff.` at any time to quit.

:- [alpha_beta].
:- [legal_moves].
:- [board].
:- [input].
:- [configurations].
:- [openings].

play :-
    write('~ Welcome to Chess! ~'), nl,
    write('Once the game starts, you will be asked to enter your move at each turn.'), nl,
    write('Each move follows the format of `piece@X/Y goto NewX/NewY`.'), nl,
    write('Each piece is represented by its standard chess notation, '), nl,
    write('with white pieces being capital letters and black pieces being lowercase letters.'), nl,
    write('You may stop at anytime by entering `ff.`'), nl,
    write('Good luck.'), nl,
    get_user_difficulty(Difficulty), !,
    get_user_colour(PlayerColour), !,
    pieces_full_board(White vs Black),
    generate_board(White vs Black, Board),
    print_board(Board),
    turn(White vs Black, Difficulty, [nomove], PlayerColour).

% turn(WhitePieces vs BlackPieces, Difficulty, PreviousMoves, Colour)
% Runs the game iteratively using last-call optimization.
% Each iteration runs a full game turn (two half-turns).

% Player is black
turn(Pieces, Difficulty, PreviousMoves, black) :-
    difficulty_to_depth(Difficulty, Pieces, Depth),
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

    !, turn(W2 vs B2, Difficulty, [PlayerMove, BotMove | PreviousMoves], black).

% Player is white
turn(Pieces, Difficulty, [LastMove | PreviousMoves], white) :-
    player_turn(Pieces, LastMove, PlayerMove, white), !,
    apply_move(Pieces, PlayerMove, W1 vs B1),
    generate_board(W1 vs B1, Board1),
    print_board(Board1),

    check_game_end(W1 vs B1, white, PlayerMove, Outcome1, _),
    handle_outcome(Outcome1, white),

    difficulty_to_depth(Difficulty, B1 vs W1, Depth),
    half_turn(B1 vs W1, Depth, black, [PlayerMove, LastMove | PreviousMoves], BotMove), !,
    apply_move(B1 vs W1, BotMove, B2 vs W2),
    generate_board(W2 vs B2, Board2),
    print_board(Board2),

    check_game_end(B2 vs W2, black, BotMove, Outcome2, _), !,
    handle_outcome(Outcome2, black),

    !, turn(W2 vs B2, Difficulty, [BotMove, PlayerMove, LastMove | PreviousMoves], white).

half_turn(Pieces, Depth, Color, [LastMove | Rest], BestMove) :-
    (
        follow_opening([LastMove | Rest], BestMove), !
        ;
        alphabeta(Depth, Pieces, Color, [LastMove | Rest], -10000, 10000, BestMove, _)
    ),
    format('Bot has made the move: ~w', [BestMove]), nl.

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
