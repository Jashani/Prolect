
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

% valid colours
colour(white).
colour(black).

% The visual representation of a piece
representation(white, rook, 'R').
representation(white, knight, 'N').
representation(white, bishop, 'B').
representation(white, queen, 'Q').
representation(white, king, 'K').
representation(white, pawn, 'P').
representation(black, rook, 'r').
representation(black, knight, 'n').
representation(black, bishop, 'b').
representation(black, queen, 'q').
representation(black, king, 'k').
representation(black, pawn, 'p').
