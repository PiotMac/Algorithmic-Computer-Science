jednokrotnie(X, L) :-
    member(X, L),
    select(X, L, Tail),
    \+ member(X, Tail).

dwukrotnie(X, L) :-
    append(A, [X|B], L),
    \+ member(X, A),
    append(C, [X|D], B),
    \+ member(X, C),
    \+ member(X, D).
