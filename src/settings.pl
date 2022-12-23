:-ensure_loaded('utils.pl').

:-dynamic board_size/1.
board_size(5).

%! step_settings(-NextState)
%
% Run a settings iteration.
%
% @param NextState To be filled with the next execution state.
step_settings(NextState):-
    print_settings,
    read(Option),
    process_settings_option(Option, NextState).

%! process_option(+Option, -NextState)
%
% Validates option from user input and retreives next state.
% Backtracks if option is invalid.
%
% @param Option Option from user input.
% @param NextState To be filled with the next execution state.
process_settings_option(Option, NextState):-
    (Option == 1 -> change_board_size, NextState = settings;
    (Option == 0 -> NextState = menu, !)).

%! print_settings
%
% Displays the settings interface.
%
print_settings:-
    print_break,
    write('\t\t Settings'), nl, nl,
    board_size(Size),
    write('1. Board Size.......................'), write(Size),
    nl, nl,
    write('1. Change board size'), nl,
    write('0. Go back'), nl.

%! change_board_size
%
% Asks for a user for a new board size. Loops while the input is not valid.
%
change_board_size:-
    nl, write('Size of the board (N*N)'), nl,
    read_number_between(2, 15, Size),
    retractall(board_size(_)),
    asserta(board_size(Size)).