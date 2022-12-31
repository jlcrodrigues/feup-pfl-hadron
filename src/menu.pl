:-ensure_loaded('utils.pl').
:-ensure_loaded('board.pl').

%! step_menu(-NextState)
%
% Run a menu iteration.
%
% @param NextState To be filled with the next execution state.
step_menu(NextState):-
    print_menu,
    read(Option),
    process_option(Option, NextState).

%! process_option(+Option, -NextState)
%
% Validates option from user input and retreives next state.
% Backtracks if option is invalid.
%
% @param Option Option from user input.
% @param NextState To be filled with the next execution state.
process_option(Option, NextState):-
    (Option == 1 -> NextState = game, !;
    (Option == 2 -> about;
    (Option == 3 -> NextState = settings;
    (Option == 0 -> NextState = exit, !)))).

process_option(_Option, NextState):- step_menu(NextState).

%! print_menu
%
% Displays the menu interface.
%
print_menu:-
    print_break,
    write(' _   _           _                       \n'),
    write('| | | |         | |                      \n'),
    write('| |_| | __ _  __| |_ __ __  _ __         \n'),
    write('|  _  |/ _  |/ _  | __/   \\|  _ \\       \n'),
    write('| | | | |_| | |_| | | | || | | | |      \n'),
    write('\\_| |_/\\__,_|\\__,_|_| \\___/|_| |_|   \n\n'),
    write('Igor Diniz             up202000162\n'),
    write('Jose Luis Rodrigues    up202008462\n\n\n'),
    write('\t1. Play'), nl,
    write('\t2. About'), nl,
    write('\t3. Settings'), nl,
    write('\t0. Exit'), nl, nl.

%! about
%
% Displays the about page.
%
about:-
    print_break,
    write('\t\t\tAbout\t\n\n'),
    write('Hadron is a two player game played on a square board,'), nl,
    write('initially empty. The two players,  Red and Blue, take'), nl,
    write('turns adding  their own tiles  to the board, one tile'), nl,
    write('per turn, starting with Red.'), nl, nl, nl,

    write('> How to place pieces?'), nl, nl,

    write('You  can  place a tile  in isolation, not adjacent to'), nl,
    write('anything.  Or  you  can  place  a  tile  to form  one'), nl,
    write('(horizontal or  vertical)  adjacency with a  friendly'), nl,
    write('tile and one with an enemy tile.  Or you can form two'), nl,
    write('adjacencies with friendly  tiles and  two adjacencies'), nl,
    write('with enemy tiles.'), nl, nl,

    SampleBoard = [[0,3,0], [2,3,1], [0,3,0]],
    print_board(SampleBoard), nl, nl,

    write('If  you are  uncertain of what to do,  you can always'), nl,
    write('type help during the game.'), nl,
    write('Hadron was created by Mark Steere,\nhttps://www.marksteeregames.com'), nl, nl,


    write('Press any key. '), read(_).

