:-use_module(library(lists)).

:-ensure_loaded('board.pl').
:-ensure_loaded('bot.pl').
:-ensure_loaded('utils.pl').

% define moving predicate for each player
:-dynamic move_term/2.

%! step_game(-NextState)
%
% Run a game iteration. This plays out a full game and then returns to the menu.
%
% @param NextState To be filled with the next execution state.
step_game(NextState):-
    board_size(BoardSize),
    initial_state(BoardSize, GameState), 
    game_loop(GameState),
    write('Press any key. '),
    read(_),
    NextState = menu.

%! initial_state(+Size, -GameState)
%
% Initializes the game state according to board size.
%
% @param Size The size of the board to be created.
% @param GameState List that holds the current game state.
initial_state(Size, GameState):-
    init_board(Board, Size),
    select_game_mode,
    GameState = [1, Board]. % blue starts

% select_game_mode
%
% Select the game mode.
select_game_mode:-
    print_break,
    write('\tSelect game mode:'), nl, nl,
    write('\t1.Player vs Player'), nl,
    write('\t2.CPU vs Player'), nl,
    write('\t3.CPU vs CPU'), nl, nl,
    read_number_between(1, 3, Mode),
    retractall(move_term),
    retractall(difficulty),
    select_game_mode(Mode).

% Player vs player
select_game_mode(1):-
    asserta(move_term(1, get_move_user)),
    asserta(move_term(2, get_move_user)).

% CPU vs player
select_game_mode(2):-
    write('Choose CPU difficulty (1-4)'), nl,
    read_number_between(1, 4, Difficulty),
    random(1, 3, BotPlayer),
    next_player(BotPlayer, UserPlayer),
    asserta(difficulty(BotPlayer, Difficulty)),
    asserta(move_term(BotPlayer, get_move_bot)),
    asserta(move_term(UserPlayer, get_move_user)).

% CPU vs CPU
select_game_mode(3):-
    write('Choose CPU difficulty for \033\[31mRed\033\[0m (1-4)'), nl,
    read_number_between(1, 4, Difficulty1),
    write('Choose CPU difficulty for \033\[34mBlue\033\[0m (1-4)'), nl,
    read_number_between(1, 4, Difficulty2),
    asserta(difficulty(1, Difficulty1)),
    asserta(difficulty(2, Difficulty2)),
    asserta(move_term(1, get_move_bot)),
    asserta(move_term(2, get_move_bot)).

%! game_loop(+GameState)
%
% Game main loop.
% For the first iteration, it gets the valid move list and calls game_loop/2, which takes over from that.
% This is done to avoid calculating the list more times then necessary.
%
% @param GameState List that holds the current game state.
game_loop(GameState):-
    [_, Board | _] = GameState,
    valid_moves(Board, ListOfMoves),
    game_loop(GameState, ListOfMoves).

% same as game_loop, but assumes the move list is already calculated
game_loop(GameState, ListOfMoves):-
    display_game(GameState),
    get_move(ListOfMoves, GameState, Move),
    move(GameState, Move, NewGameState),
    [_, NewBoard | _] = NewGameState,
    valid_moves(NewBoard, NewListOfMoves),
    game_over(NewGameState, _Winner, NewListOfMoves).

%! display_game(+GameState)
%
% Display the current game status, including board and turn.
%
% @param GameState List that holds the current game state.
display_game(GameState):-
    [Player, Board | _] = GameState,
    print_break,
    print_board(Board), nl,
    (Player == 1 -> write('\033\[31mPlayer 1 playing\033\[0m'); %red
    (Player == 2 -> write('\033\[34mPlayer 2 playing\033\[0m'))). %blue

%! game_over(+GameState, -Winner, +ListOfMoves)
% 
% Check if the game is over.
% For the game to be over, there can not exist any valid move left.
%
% @param GameState List that holds the current game state.
% @param ListOfMoves Previously calculated list of allowed moves.
game_over(GameState, Winner, ListOfMoves):-
    ListOfMoves == [],
    [Player, Board | _] = GameState,
    next_player(Player, Winner),
    print_break,
    print_board(Board), nl,
    nl,
    (Winner == 1 -> write('\033\[31mPlayer 1 wins!!!\033\[0m'); %red
    (Winner == 2 -> write('\033\[34mPlayer 2 wins!!!\033\[0m'))), %blue
    nl, nl.

% In case the game is not over, this predicate calls game_loop so the game goes on.
game_over(GameState, _Winner, ListOfMoves):-
    ListOfMoves \== [],
    game_loop(GameState, ListOfMoves).

%! get_move(+GameState, -NewGameState)
%
% Gets the next move, either from player or bot.
% To distinguish them, it queries what is the current move_term and calls it.
%
% @param ListOfMoves Previously calculated list of allowed moves.
% @param GameState List that holds the current game state.
% @param Move User move parsed to proper int board indexes.
get_move(ListOfMoves, GameState, Move):-
    ListOfMoves \== [],
    [Player | _] = GameState,
    move_term(Player, MoveTerm),

    call(MoveTerm, ListOfMoves, GameState, Move).

%! get_move(+GameState, -NewGameState, -ListOfMoves)
%
% Reads a move from user input. Loops until it finds a valid move.
%
% @param ListOfMoves Previously calculated list of allowed moves.
% @param _GameState This is being ignored but we want the same prototype as get_move_bot.
% @param Move User move parsed to proper int board indexes.
get_move_user(ListOfMoves, _GameState, Move):-
    read(MoveChar),
    move_options(MoveChar, ListOfMoves),
    parse_move(MoveChar, Move),
    member(Move, ListOfMoves),
    !.

% Used to backtrack.
get_move_user(ListOfMoves, _GameState, Move):-
    get_move_user(ListOfMoves, _, Move).

%! parse_move(+MoveChar, -Move)
%
% Converts a user input move to the internal representation, e.g. b1 to 2,0.
%
% @param MoveChar Move from user input.
% @param Move User move parsed to proper int board indexes.
parse_move(MoveChar, Move):-
    \+ integer(MoveChar),
    atom_chars(MoveChar, MoveList),
    [RowChar, ColChar | _] = MoveList,
    char_code('a', A), char_code('1', One),
    char_code(RowChar, RowCode), char_code(ColChar, ColCode), 
    Row is RowCode - A, Col is ColCode - One,
    Move = [Row, Col].

%! move(+GameState, +Move, -NewGameState)
%
% Perform a move and change the current game state.
% This should be called after the move is validated by other move predicates.
%
% @param GameState List that holds the current game state.
% @param Move Validated move input.
% @param NewGameState List that holds the game state after moving.
move(GameState, Move, NewGameState):-
    [Player, Board] = GameState,
    next_player(Player, NextPlayer),
    move_board(Board, Move, NextBoard, Player),
    NewGameState = [NextPlayer, NextBoard].

% Quit the execution of the game
move_options('quit', _):- break.

%! move_options('help', ListOfMoves).
%
% Display help menu to the user mid game.
% 
% @param ListOfMoves list of valid moves (will be displayed).
move_options('help', ListOfMoves):-
    nl,
    write('These are the availabe moves at the moment [row, col]:'), nl, nl,
    print_moves(ListOfMoves), nl, nl,
    write('Write `quit` to exit the game.'), nl, nl.

% Succeeds if the option is not defined.
move_options(Option, _):-
    Option \== 'quit', 
    Option \== 'help'.

%! print_moves(ListOfMoves).
%
% Recursively loop and write a list of moves.
%
% @param ListOfMoves list of valid moves (will be displayed).
print_moves(ListOfMoves):-
    ListOfMoves \== [],
    [Move | Remaining] = ListOfMoves,
    [RowN , ColN | _] = Move,

    Col is ColN + 1,
    char_code('a', A),
    RowCode is A + RowN,
    char_code(Row, RowCode),

    write(Row), write(Col), write(' '),
    print_moves(Remaining).

print_moves([]).
