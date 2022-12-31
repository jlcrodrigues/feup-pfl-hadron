:- use_module(library(random)).
:-ensure_loaded('board.pl').

:- dynamic difficulty/2.

% get_move_bot(+ListOfMoves, +GameState, -Move)
%
% Gets the move from the AI.
% This predicate fetches the diffculty from the database and calls choose_move with it.
%
% @param ListOfMoves Previously calculated list of allowed moves.
% @param GameState List that holds the current game state.
% @param Move chose by the algorithm.
get_move_bot(_ListOfMoves, GameState, Move):-
    [Player | _] = GameState,
    difficulty(Player, Difficulty),
    choose_move(GameState, Player, Difficulty, Move).

%! value(+GameState, +Player, -Value):-
% 
% Get the value of a board.
% A board is better then another if the avaible amount of moves is less.
% Thus, a smaller Value means a better board.
%
% @param GameState List that holds the current game state.
% @param Player 1 or 2 to represent the turn. Not being used atm.
% @param Value The value of the board (less is better).
value(GameState, _Player, Value):-
    [_, Board | _] = GameState,
    valid_moves(Board, ListOfMoves),
    length(ListOfMoves, Value).
    
% choose_move(+GameState, +Player, +Level, -Move)
%
%
%
% @param GameState List that holds the current game state.
% @param Player 1 or 2 to represent the turn.
% @param Level Difficulty level for the bot.
% @param Move chose by the algorithm.
choose_move([_Player, Board], _Player, 1, Move):-
    % difficulty 1 is just a random move
    valid_moves(Board, ListOfMoves),
    random_member(Move, ListOfMoves).

choose_move(GameState, _Player, Level, Move):-
    Level > 0, Level < 5,
    NLevel is Level - 1, % 1 is the min depth but 2 is min difficulty
    minimax(GameState, NLevel, Move).

test:-
    GameState = [1, [[0,2,0], [1,1,0], [0,0,2]]],
    minimax(GameState, 5, Move),
    write(Move).

% entry point
minimax([Player, Board], Depth, Move):-
    next_player(Player, MaxPlayer), % the player to maximize is the opponent -a less value is better
    minimax([Player, Board], Depth, MaxPlayer, Move, _Value).

minimax(GameState, Depth, MaxPlayer, Move, Value):-
    [_, Board | _] = GameState,
    valid_moves(Board, ListOfMoves),
    minimax(GameState, ListOfMoves, Depth, MaxPlayer, Move, Value).

% base case: game is over
minimax(_GameState, [], _Depth, _MaxPlayer, _Move, Value):-
    Value is 0.

% base case: depth is zero
minimax(GameState, _ListOfMoves, 0, _MaxPlayer, _Move, Value):-
    value(GameState, _, Value).

% recursive case: Maximizing player
minimax([Player, Board], ListOfMoves, Depth, MaxPlayer, Move, Value):-
    Depth > 0,
    Player == MaxPlayer,
    minimax_aux([Player, Board], ListOfMoves, Depth, MaxPlayer, ListOfValues),

    %get max and max index
    max_member(Value, ListOfValues),
    nth0(Idx, ListOfValues, Value),

    %get max move
    nth0(Idx, ListOfMoves, Move).

% recursive case: Minimizing player
minimax([Player, Board], ListOfMoves, Depth, MaxPlayer, Move, Value):-
    Depth > 0,
    Player \== MaxPlayer,
    minimax_aux([Player, Board], ListOfMoves, Depth, MaxPlayer, ListOfValues),

    %get min and min index
    min_member(Value, ListOfValues),
    nth0(Idx, ListOfValues, Value),

    %get min move
    nth0(Idx, ListOfMoves, Move).


minimax_aux([Player, Board], ListOfMoves, Depth, MaxPlayer, ListOfValues):-
    ListOfMoves \== [],
    [Move | RemainingMoves] = ListOfMoves,
    minimax_aux([Player, Board], RemainingMoves, Depth, MaxPlayer, PrevValues),

    NDepth is Depth - 1,
    move_board(Board, Move, NBoard, Player),
    next_player(Player, NPlayer),
    NGameState = [NPlayer, NBoard],

    minimax(NGameState, NDepth, MaxPlayer, _Move, Value),
    ListOfValues = [Value | PrevValues].

minimax_aux(_GameState, [], _Depth, _MaxPlayer, ListOfValues):-
    ListOfValues = [].
