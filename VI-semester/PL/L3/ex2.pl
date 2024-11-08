max_sum(L, S) :-
    max_sum(L, 0, 0, S).

max_sum([], _, MaxSofar, MaxSofar).
max_sum([H|T], CurrentSum, MaxSofar, MaxSum) :-
    % Oblicz nową sumę sekcji z obecnym elementem
    NewSum is max(0, CurrentSum + H),
    % Aktualizuj wartość największej sumy
    NewMaxSofar is max(MaxSofar, NewSum),
    % Rekurencyjnie analizuj resztę listy
    max_sum(T, NewSum, NewMaxSofar, MaxSum).
