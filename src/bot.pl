:- use_module(library(random)).


get_move_bot(ListOfMoves, _GameState, Move):-
    random_member(Move, ListOfMoves).
    