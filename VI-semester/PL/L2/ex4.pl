lista(N, X) :-
    Len is N * 2,
    length(X, Len),
    pomiędzy(N, X).

pomiędzy(1, X) :-
    zweryfikuj(1, X).

pomiędzy(N, X) :-
    zweryfikuj(N, X),
    A is N - 1,
    pomiędzy(A, X).

zweryfikuj(N, X) :-
    nth0(A, X, N),
    nth0(B, X, N),
    A < B,
    (B - A) mod 2 =:= 1,
    Min is N - 1,
    Max is 2 * Min,
    between(Min, Max, A).
