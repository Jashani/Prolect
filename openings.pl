:- [operators].

% Find the next move based on some opening
follow_opening(PreviousMoves, NextMove) :-
    possible_openings(PreviousMoves, Openings),
    possible_moves(PreviousMoves, Openings, Moves),
    random_member(NextMove, Moves), !.

% Get all openings relevant to previous moves
possible_openings(PreviousMoves, RelevantOpenings) :-
    findall(Opening, opening(Opening), Openings),
    include(subset(PreviousMoves), Openings, RelevantOpenings).

% Get all possible next moves from given openings 
possible_moves(_, [], []).
possible_moves(PreviousMoves, [Opening | Openings], Moves) :-
    possible_moves(PreviousMoves, Openings, PotentialMoves),
    (
        next_move(PreviousMoves, Opening, PotentialMove), !,
        Moves = [PotentialMove | PotentialMoves]
        ;
        Moves = PotentialMoves
    ).

% Get next move from opening
next_move(PreviousMoves, [Move | Opening], NextMove) :-
    msort(PreviousMoves, Sorted),
    msort(Opening, Sorted),
    NextMove = Move
    ;
    next_move(PreviousMoves, Opening, NextMove).

% Opening are read from right to left to match move order

% e4 openings
opening([bishop@6/1 goto 2/5, knight@2/8 goto 3/6, knight@7/1 goto 6/3, pawn@5/7 goto 5/5, pawn@5/2 goto 5/4, nomove]). % Ruy Lopez
opening([bishop@6/1 goto 3/4, knight@2/8 goto 3/6, knight@7/1 goto 6/3, pawn@5/7 goto 5/5, pawn@5/2 goto 5/4, nomove]). % Italian
opening([pawn@6/2 goto 6/4, knight@7/8 goto 6/6, knight@2/1 goto 3/3, pawn@5/7 goto 5/5, pawn@5/2 goto 5/4, nomove]). % Vienna gambit my love
opening([bishop@6/1 goto 3/4, knight@2/8 goto 3/6, knight@2/1 goto 3/3, pawn@5/7 goto 5/5, pawn@5/2 goto 5/4, nomove]). % Vienna not so gambit
opening([pawn@4/7 goto 4/5, pawn@4/2 goto 4/4, pawn@5/7 goto 5/6, pawn@5/2 goto 5/4, nomove]). % French (stinky)
opening([pawn@5/4 goto 5/5, pawn@4/7 goto 4/5, pawn@4/2 goto 4/4, pawn@3/7 goto 3/6, pawn@5/2 goto 5/4, nomove]). % Caro kann - advanced
opening([pawn@6/2 goto 6/3, pawn@4/7 goto 4/5, pawn@4/2 goto 4/4, pawn@3/7 goto 3/6, pawn@5/2 goto 5/4, nomove]). % Caro kann - fantasy!!!
opening([pawn@4/7 goto 4/5, pawn@5/2 goto 5/4, nomove]). % Scandinavian
opening([pawn@4/7 goto 4/6, knight@7/1 goto 6/3, pawn@3/7 goto 3/5, pawn@5/2 goto 5/4, nomove]). % Sicillian - Main
opening([knight@2/8 goto 3/6, knight@7/1 goto 6/3, pawn@3/7 goto 3/5, pawn@5/2 goto 5/4, nomove]). % Sicillian - Old
opening([knight@2/1 goto 3/3, pawn@3/7 goto 3/5, pawn@5/2 goto 5/4, nomove]). % Sicillian - Closed

% d4 openings
opening([pawn@3/7 goto 3/6, pawn@3/2 goto 3/4, pawn@4/7 goto 4/5, pawn@4/2 goto 4/4, nomove]). % Queen's gambit - Slav
opening([pawn@5/7 goto 5/6, pawn@3/2 goto 3/4, pawn@4/7 goto 4/5, pawn@4/2 goto 4/4, nomove]). % Queen's gambit declined
opening([pawn@4/5 goto 3/4, pawn@3/2 goto 3/4, pawn@4/7 goto 4/5, pawn@4/2 goto 4/4, nomove]). % Queen's gambit ACCEPTED!!?!?
opening([knight@2/8 goto 3/6, pawn@3/2 goto 3/4, pawn@4/7 goto 4/5, pawn@4/2 goto 4/4, nomove]). % Queen's gambit - CHIGORIN LET'S GO
opening([pawn@7/7 goto 7/6, pawn@3/2 goto 3/4, knight@7/8 goto 6/6, pawn@4/2 goto 4/4, nomove]). % Indian
opening([knight@7/1 goto 6/3, knight@7/8 goto 6/6, bishop@3/1 goto 6/4, pawn@4/7 goto 4/5, pawn@4/2 goto 4/4, nomove]). % London
opening([knight@7/1 goto 6/3, pawn@4/7 goto 4/5, pawn@4/2 goto 4/4, nomove]). % Zukertort
opening([bishop@6/1 goto 7/2, knight@7/8 goto 6/6, pawn@7/2 goto 7/3, pawn@6/7 goto 6/5, pawn@4/2 goto 4/4, nomove]). % Dutch + Fianchetto

% other exciting things
opening([knight@7/8 goto 6/6, knight@7/1 goto 6/3, nomove]). % Reti
opening([pawn@5/7 goto 5/5, pawn@3/2 goto 3/4, nomove]). % Dutch - king's pawn
opening([knight@7/8 goto 6/6, pawn@3/2 goto 3/4, nomove]). % Dutch - knight
