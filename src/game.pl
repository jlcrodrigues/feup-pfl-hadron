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
    initial_state(BoardSize, GameState), %todo board size
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
    retractall(move_term),
    asserta(move_term(1, get_move_user)),
    asserta(move_term(2, get_move_bot)),
    GameState = [1, Board].

%! game_loop(+GameState)
%
% Game main loop.
% For the first iteration, it gets the valid move list and calls game_loop/2, which takes over from that.
% This is done to avoid calculating the list more times then necessary.
%
% @param GameState List that holds the current game state.
game_loop(GameState):-
    valid_moves(GameState, ListOfMoves),
    game_loop(GameState, ListOfMoves).

% same as game_loop, but assumes the move list is already calculated
game_loop(GameState, ListOfMoves):-
    display_game(GameState),
    get_move(ListOfMoves, GameState, Move),
    move(GameState, Move, NewGameState),
    valid_moves(NewGameState, NewListOfMoves),
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

%! valid_moves(+Row, +Col, +GameState, -ListOfMoves)
%
% Find the list of valid moves for a game state.
% Loops every possible coordinate recursively with Row and Col.
% 
% @param Row Number of the row in which the cell is located.
% @param Col Number of the column in which the cell is located.
% @param GameState List that holds the current game state.
% @param ListOfMoves List of possible moves.
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

% Starting point for the recursion, see valid_moves\4.
valid_moves(GameState, ListOfMoves):-
    valid_moves(0, 0, GameState, ListOfMoves).

%! validate_move(+Row, +Col, +Board, +ListOfMoves, -NewListOfMoves)
%
% Check if a move to (Row, Col) is valid.
% To validate a move, first the position has to be empty. 
% Moreover, it gets the orthogonally adjacent cells lists and checks for equality (see game rules).
%
% @param Row Number of the row in which the cell is located.
% @param Col Number of the column in which the cell is located.
% @param Board List of Lists to represent the board.
% @param ListOfMoves Previous list of possible moves.
% @param NewListOfMoves Updated list of possible moves.
validate_move(Row, Col, Board, ListOfMoves, NewListOfMoves):-
    %verify if the cell is empty
    nth0(Row, Board, RowList),
    nth0(Col, RowList, Target),
    Target == 0,

    % count occurrences of each piece on the adjacent cells
    get_adjacent(Row, Col, Board, Adjacent),
    count2(1, 2, Adjacent, Count1, Count2),
    Count1 == Count2,

    Move = [Row, Col],
    NewListOfMoves = [Move | ListOfMoves],
    !.

% used for backtracking
validate_move(_Row, _Col, _Board, ListOfMoves, NewListOfMoves):- 
    NewListOfMoves = ListOfMoves.


%! get_adjacent(+Row, +Col, +Board, -Adjacent):-
%
% Gets orthogonally adjacent cells to a coordinate.
% Because of edges and corners, it calls a functor for each of the four cells (top, right, left, bottom).
% It then groups all 4 in a list.
%
% @param Row Number of the row in which the cell is located.
% @param Col Number of the column in which the cell is located.
% @param Board List of Lists to represent the board.
% @param Adjacent List with all four orthogonally adjacent cells.
get_adjacent(Row, Col, Board, Adjacent):-
    get_adjacent_top(Row, Col, Board, Top),
    get_adjacent_right(Row, Col, Board, Right),
    get_adjacent_left(Row, Col, Board, Left),
    get_adjacent_bot(Row, Col, Board, Bot),
    Adjacent = [Top, Right, Left, Bot].

%! get_adjacent_top(+Row, +Col, +Board, -Top):-
%
% Retreives the cell above the one on (Row, Col).
% If it doesn't exists returns 0.
%
% @param Row Number of the row in which the cell is located.
% @param Col Number of the column in which the cell is located.
% @param Board List of Lists to represent the board.
% @param Top Value on the cell above. Zero if empty or doesn't exist.
get_adjacent_top(Row, Col, Board, Top):-
    Row > 0, 
    RowTop is Row - 1,
    nth0(RowTop, Board, RowList),
    nth0(Col, RowList, Top).

% Used for backtracking
get_adjacent_top(Row, _Col, _Board, Top):-
    Row < 1, Top = 0.

%! get_adjacent_bot(+Row, +Col, +Board, -Bot):-
%
% Retreives the cell bellow the one on (Row, Col).
% If it doesn't exists returns 0.
%
% @param Row Number of the row in which the cell is located.
% @param Col Number of the column in which the cell is located.
% @param Board List of Lists to represent the board.
% @param Bot Value on the cell bellow. Zero if empty or doesn't exist.
get_adjacent_bot(Row, Col, Board, Bot):-
    length(Board, Size),
    Row < Size - 1, 
    RowBot is Row + 1,
    nth0(RowBot, Board, RowList),
    nth0(Col, RowList, Bot).

% Used for backtracking
get_adjacent_bot(Row, _Col, Board, Bot):-
    length(Board, Size),
    Row >= Size - 1, Bot = 0.

%! get_adjacent_right(+Row, +Col, +Board, -Right):-
%
% Retreives the cell to the right of the one on (Row, Col).
% If it doesn't exists returns 0.
%
% @param Row Number of the row in which the cell is located.
% @param Col Number of the column in which the cell is located.
% @param Board List of Lists to represent the board.
% @param Right Value on the cell to the right. Zero if empty or doesn't exist.
get_adjacent_right(Row, Col, Board, Right):-
    length(Board, Size),
    Col < Size - 1, 
    ColRight is Col + 1,
    nth0(Row, Board, RowList),
    nth0(ColRight, RowList, Right).

% Used for backtracking
get_adjacent_right(_Row, Col, Board, Right):-
    length(Board, Size),
    Col >= Size - 1, Right = 0.

%! get_adjacent_left(+Row, +Col, +Board, -Left):-
%
% Retreives the cell to the left of the one on (Row, Col).
% If it doesn't exists returns 0.
%
% @param Row Number of the row in which the cell is located.
% @param Col Number of the column in which the cell is located.
% @param Board List of Lists to represent the board.
% @param Left Value on the cell to the left. Zero if empty or doesn't exist.
get_adjacent_left(Row, Col, Board, Left):-
    Col > 0,
    ColLeft is Col - 1,
    nth0(Row, Board, RowList),
    nth0(ColLeft, RowList, Left).

% Used for backtracking
get_adjacent_left(_Row, Col, _Board, Left):-
    Col < 1, Left = 0.
