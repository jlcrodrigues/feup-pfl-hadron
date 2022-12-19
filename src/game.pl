:-ensure_loaded('board.pl').

step_game(NextState):-
    NextState = menu,
    write('This is the game state'), nl.
