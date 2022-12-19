:-use_module(library(lists)).

:-ensure_loaded('board.pl').
:-ensure_loaded('utils.pl').

%! step_game(-NextState)
%
% Run a game iteration. This plays out a full game and then returns to the menu.
%
% @param NextState To be filled with the next execution state.
step_game(NextState):-
    initial_state(3, GameState), %todo board size
    game_loop(GameState),
    NextState = menu.

%! initial_state(+Size, -GameState)
%
% Initializes the game state according to board size.
%
% @param Size The size of the board to be created.
% @param GameState List that holds the current game state.
initial_state(Size, GameState):-
    init_board(Board, Size),
    GameState = [1, Board].

%! display_game(+GameState)
%
% Display the current game status, including board and turn.
%
% @param GameState List that holds the current game state.
display_game(GameState):-
    print_break,
    [Player, Board | _] = GameState,
    print_break,
    print_board(Board),
    format('Player ~d`s turn.~n', [Player]).

%! game_loop(+GameState)
%
% Game main loop.
%
% @param GameState List that holds the current game state.
game_loop(GameState):-
    display_game(GameState),
    get_move(GameState, NextGameState),
    game_loop(NextGameState).

%! get_move(+GameState, -NewGameState)
%
% Ask for a move and change game state accordingly.
% This predicate gets the list of valid moves and then calls get_move\3 to avoid repetition.
%
% @param GameState List that holds the current game state.
% @param NewGameState List that holds the game state after moving.
get_move(GameState, NewGameState):-
    valid_moves(GameState, ListOfMoves),
    get_move(GameState, NewGameState, ListOfMoves).

%! get_move(+GameState, -NewGameState, -ListOfMoves)
%
% Reads a move from user input.
% Sends the input to check_move\4 so it can validate or loop.
%
% @param GameState List that holds the current game state.
% @param NewGameState List that holds the game state after moving.
% @param ListOfMoves Previously calculated list of allowed moves.
get_move(GameState, NewGameState, ListOfMoves):-
    read(MoveChar),
    parse_move(MoveChar, Move),
    check_move(GameState, Move, NewGameState, ListOfMoves).

%! parse_move(+MoveChar, -Move)
%
% Converts a user input move to the internal representation, e.g. b1 to 2,0.
%
% @param MoveChar Move from user input.
% @param Move User move parsed to proper int board indexes.
parse_move(MoveChar, Move):-
    atom_chars(MoveChar, MoveList),
    [RowChar, ColChar | _] = MoveList,
    char_code('a', A), char_code('1', One),
    char_code(RowChar, RowCode), char_code(ColChar, ColCode), 
    Row is RowCode - A, Col is ColCode - One,
    Move = [Row, Col].

%! check_move(+GameState, +Move, -NewGameState, -ListOfMoves)
%
% Returns true when a valid move is found and backtracks to get_move is the move was invalid.
%
% @param GameState List that holds the current game state.
% @param Move Move to be validated.
% @param NewGameState List that holds the game state after moving.
% @param ListOfMoves Previously calculated list of allowed moves.
check_move(GameState, Move, NewGameState, ListOfMoves):-
    member(Move, ListOfMoves),
    move(GameState, Move, NewGameState),
    !.

% Used to backtrack when the move is not valid.
check_move(GameState, Move, NewGameState, ListOfMoves):-
    get_move(GameState, NewGameState, ListOfMoves).

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

%! move_board(+Board, +Move, -NewBoard, +Player).
%
% Creates a new board according to a movement.
%
% @param Board List of Lists to represent the board.
% @param Move Validated move input.
% @param NewBoard List of Lists to represent the board after moving.
% @param Player Symbol that represnts who played the move.
move_board(Board, Move, NewBoard, Player):-
    length(Board, BoardSize),
    Size is BoardSize - 1,
    (foreach(OldRow, Board), foreach(NewRow, NewBoard), for(Row, 0, Size), param(Size, Move, Player) do
        (foreach(OldCol, OldRow), foreach(NewCol, NewRow), for(Col, 0, Size), param(Row, Move, Player) do 
            [MoveRow, MoveCol] = Move,
            (MoveRow == Row, MoveCol == Col -> NewCol = Player ; NewCol = OldCol)
        )
    ).

%! next_player(+Player, -NextPlayer)
%
% Switches to the next player.
%
% @param Player Current player.
% @param Player Next player.
next_player(1, Next):- Next = 2.
next_player(2, Next):- Next = 1.

test:-
    GameState = [1, [[0,0,0], [1,0,2],[0,0,0]]],
    valid_moves(GameState, L),
    write(L).

valid_moves(GameState, ListOfMoves):-
    valid_moves(0, 0, GameState, ListOfMoves).

valid_moves(Row, Col, GameState, ListOfMoves):-
    [_, Board | _] = GameState,
    length(Board, Size),
    (Row == Size -> ListOfMoves = [], !;
    (Col == Size -> NextRow is Row + 1, valid_moves(NextRow, 0, GameState, ListOfMoves);
    (
        NextCol is Col + 1,
        valid_moves(Row, NextCol, GameState, List),

        validate_move(Row, Col, Board, List, ListOfMoves)
    ))).


validate_move(Row, Col, Board, ListOfMoves, NewListOfMoves):-
    %get_adjacent(Row, Col, Board, Adjacent),
    nth0(Row, Board, RowList),
    nth0(Col, RowList, Target),
    Target == 0,

    Move = [Row, Col],
    NewListOfMoves = [Move | ListOfMoves],
    !.

validate_move(Row, Col, Board, ListOfMoves, NewListOfMoves):- 
    NewListOfMoves = ListOfMoves.

get_adjacent(Row, Col, Board, Adjacent):-
    RowTop is Row + 1,
    nth0(RowTop, Board, RowList),
    nth0(Col, RowList, Top),
    write(Row), pad,  write(Col), pad, write(Top), nl.

get_adjacent_Top(Row, Col, Board, PrevAdjacent, Adjacent):-
    RowTop is Row + 1.

