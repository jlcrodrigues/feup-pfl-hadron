
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
    print_board_line(Board, top_left, top, top_right), nl,
    [Head | Tail] = Board, print_board_row(Head), nl, 
    (foreach(Row, Tail), param(Board) do
        print_board_line(Board, left_side, mid, right_side), nl, print_board_row(Row), nl
    ),
    print_board_line(Board, bot_left, bot, bot_right), nl.

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
    (Symbol == 1 -> write('\033\[31mO\033\[0m'), !; %red
    (Symbol == 2 -> write('\033\[34mO\033\[0m'), !))). %blue


%% Special characters
%
% All the terms below define chars to be used to print the board.
%
pad:-
    write(' ').

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
