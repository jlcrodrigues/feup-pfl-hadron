:-ensure_loaded('utils.pl').

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
    (Option == 0 -> NextState = exit, !)).

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
    write('\t1. Play'), nl,
    write('\t0. Exit'), nl.