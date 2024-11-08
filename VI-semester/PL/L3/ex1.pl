suma_listy([], 0).
suma_listy([X | L], Suma) :-
    suma_listy(L, Reszta),
    Suma is X + Reszta.
długość_listy([], 0).
długość_listy([_|L], Długość) :-
    długość_listy(L, Reszta),
    Długość is Reszta + 1.
średnia(L, Średnia) :-
    suma_listy(L, Suma),
    długość_listy(L, Długość),
    Długość > 0,
    Średnia is Suma / Długość.
suma_odchyleń([], _, 0).
suma_odchyleń([X|L], Średnia, Wynik) :-
    suma_odchyleń(L, Średnia, Reszta),
    Wynik is Reszta + (X - Średnia)^2.
wariancja([], 0).
wariancja(L, D) :-
    długość_listy(L, Długość),
    Długość > 0,
    średnia(L, Średnia),
    suma_odchyleń(L, Średnia, Wynik),
    D is Wynik / Długość.
