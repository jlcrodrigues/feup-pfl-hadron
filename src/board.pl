:-ensure_loaded('utils.pl').
:-use_module(library(lists)).

%! init_board(-Board, +BoardSize)
%
% Initializes the Board to its starting state (all empty).
% 
% @param Board List of Lists to represent the board.
% @param BoardSize Size of the board to create.
init_board(Board, BoardSize):-
    (for(_, 1, BoardSize), foreach(Row, Board), param(BoardSize) do
        (for(_, 1, BoardSize), foreach(Col, Row) do
            Col is 0
        )
    ).

%! print_board(+Board)
%
% Displays the board to stdout.
%
% @param Board List of Lists that represents the board.
print_board(Board):-
    center, print_board_line(Board, top_left, top, top_right), nl,
    [Head | Tail] = Board, center, print_board_row(Head), write(' a'), nl, 
    length(Board, Length),
    (foreach(Row, Tail), for(X, 1, Length - 1), param(Board) do
        center, print_board_line(Board, left_side, mid, right_side), nl,
        char_code('a', A),
        I is A + X,
        char_code(Idx, I),
        center, print_board_row(Row), pad, write(Idx), nl
    ),
    center, print_board_line(Board, bot_left, bot, bot_right), nl,
    center, print_col_index(Board).

%! print_board_line(+Board, +Left, +Mid, +Right)
%
% Prints a line separator with the needed size.
%
% @param Board List of Lists that represents the board.
% @param Left Left character.
% @param Mid Middle character.
% @param Right Right character.
print_board_line(Board, Left, Mid, Right):-
    call(Left), 
    [_ | Tail] = Board, horiz, horiz, horiz,
    (foreach(_, Tail), param(Mid) do call(Mid), horiz, horiz, horiz),
    call(Right).

%! print_board_row(+Board)
%
% Print a line from the Board list with the respective symbols.
%
% @param Board List of Lists that represents the board.
print_board_row(Row):-
    vert,
    (foreach(Col, Row) do pad, print_symbol(Col), pad, vert).

%! print_symbol(+Symbol)
%
% Print player's symbol.
%
% @param Symbol Either 0, 1 or 2 for empty, player 1 and player 2, respectively.
print_symbol(Symbol):-
    (Symbol == 0 -> write(' '), !;
    (Symbol == 1 -> write('\033\[31m'), player, write('\033\[0m'), !; %red
    (Symbol == 2 -> write('\033\[34m'), player, write('\033\[0m'), !; %blue
    (Symbol == 3 -> write('\033\[32m'), check, write('\033\[0m'), !)))). %green check

%! print_col_index(+Board)
%
% Print the column index for the board.
%
% @param Board List of Lists that represents the board.
print_col_index(Board):-
    length(Board, Length),
    (for(X, 1, Length) do
        pad, pad, write(X), pad
    ),
    nl.

%% Special characters
%
% All the terms below define chars to be used to print the board.
%
pad:-
    write(' ').

center:-
    write('\t\t    ').

top_left :-
	write('\x250C\').

top :-
	write('\x252C\').

top_right :-
	write('\x2510\').

left_side :-
	write('\x251C\').

mid :-
	write('\x253C\').

right_side :-
	write('\x2524\').

bot_left :-
	write('\x2514\').

bot :-
	write('\x2534\').

bot_right :-
	write('\x2518\').

vert :-
	write('\x2502\').

horiz :-
	write('\x2500\').

check :-
	write('\x2713\').

player :-
	write('\x25ef\').

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
% Move validation

%! valid_moves(+Row, +Col, +Board, -ListOfMoves)
%
% Find the list of valid moves for a game state.
% Loops every possible coordinate recursively with Row and Col.
% 
% @param Row Number of the row in which the cell is located.
% @param Col Number of the column in which the cell is located.
% @param Board List that holds the board.
% @param ListOfMoves List of possible moves.
valid_moves(Row, Col, Board, ListOfMoves):-
    length(Board, Size),
    (Row == Size -> ListOfMoves = [], !;
    (Col == Size -> NextRow is Row + 1, valid_moves(NextRow, 0, Board, ListOfMoves);
    (
        NextCol is Col + 1,
        valid_moves(Row, NextCol, Board, List),

        validate_move(Row, Col, Board, List, ListOfMoves)
    ))).

% Starting point for the recursion, see valid_moves\4.
valid_moves(Board, ListOfMoves):-
    valid_moves(0, 0, Board, ListOfMoves).

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