środkowy([X], X).
środkowy(L, X) :-
    length(L, Len),
    Len mod 2 =:= 1,
    append(L1, [X|L2], L),
    length(L1, A),
    length(L2, B),
    A =:= B.
