:- use_module(library(random)).

:- dynamic difficulty/2.

% get_move_bot(+ListOfMoves, +GameState, -Move)
%
% Gets the move from the AI.
% This predicate fetches the diffculty from the database and calls choose_move with it.
%
% @param ListOfMoves Previously calculated list of allowed moves.
% @param GameState List that holds the current game state.
% @param Move chose by the algorithm.
get_move_bot(ListOfMoves, GameState, Move):-
    [Player | _] = GameState,
    difficulty(Player, Difficulty),
    choose_move(GameState, Player, Difficulty, ListOfMoves, Move).

% choose_move(+GameState, +Player, +Level, -Move)
%
% There's no point in using this version or else we would have to re-calculate valid moves.
choose_move(_GameState, _Player, _Level, _Move).

% choose_move(+GameState, +Player, +Level, -Move)
%
%
%
% @param GameState List that holds the current game state.
% @param Player 1 or 2 to represent the turn.
% @param Level Difficulty level for the bot.
% @param ListOfMoves Previously calculated list of allowed moves.
% @param Move chose by the algorithm.
choose_move(_GameState, _Player, 1, ListOfMoves, Move):-
    % difficulty 1 is just a random move
    random_member(Move, ListOfMoves).



% TODO find a way to evaluate the board, this will be used for the harder difficulties
% 1- random, 2-greedy, 3+ - minimax
value(+GameState, +Player, -Value).
    