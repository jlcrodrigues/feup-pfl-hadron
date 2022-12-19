:-ensure_loaded('menu.pl').
:-ensure_loaded('game.pl').

% step(+State, -NextState)
%
% Redirects execution to the correct state.
% The specific state function will return the next state.
%
% @param State Current execution state.
% @param NextState To be filled with the next execution state.
step(menu, NextState):- step_menu(NextState).
step(game, NextState):- step_game(NextState).

%! play
%
% Game start predicate.
%
play:-
    play(menu).

%! play(-State)
%
% Game main loop. This will be called by play with the starting state.
%
% @param State The current execution state.
play(State):-
    step(State, NextState),
    play(NextState).
