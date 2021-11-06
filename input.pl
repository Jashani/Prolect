
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
