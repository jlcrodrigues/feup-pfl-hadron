%! print_break
%
% Used to print multiple new lines to the screen in order to change between pages.
%
print_break:-
    count(_, 0, 20) do nl.