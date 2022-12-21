%! print_break
%
% Used to print multiple new lines to the screen in order to change between pages.
%
print_break:-
    count(_, 0, 30) do nl.

%! count2(+X, +Y, +List, -CountX, -CountY)
%
% Count occurrences of X and Y in a list.
%
% @param X First element to check for occurrences.
% @param Y Second element to check for occurrences.
% @param List List to be searched.
% @param CountX Count of occurrences of the first element.
% @param CountY Count of occurrences of the second element.
count2(X, Y, List, CountX, CountY):-
    List \= [],
    [Head | Tail] = List,
    count2(X, Y, Tail, PrevCountX, PrevCountY),
    (Head == X -> CountX is PrevCountX + 1, CountY = PrevCountY;
    (Head == Y -> CountY is PrevCountY + 1, CountX = PrevCountX;
    CountX = PrevCountX, CountY = PrevCountY)).

% base case
count2(_X, _Y, [], CountX, CountY):-
    CountX = 0, CountY = 0.

