%   +---1--+---2--+---3--+
%   |      |      |      |
%   4      5      6      7
%   |      |      |      |
%   +---8--+---9--+--10--+
%   |      |      |      |
%   11     12     13    14
%   |      |      |      |
%   +--15--+--16--+--17--+
%   |      |      |      |
%   18     19     20    21
%   |      |      |      |
%   +--22--+--23--+--24--+


małe_kwadraty(Licznik, Wynik) :-
    ((MałyKwadrat1 = [1, 4, 5, 8], Licznik1 = 1) ; (MałyKwadrat1 = [], Licznik1 = 0)),
    ((union(MałyKwadrat1, [2, 5, 6, 9], MałyKwadrat2), Licznik2 is Licznik1 + 1) ; (MałyKwadrat2 = MałyKwadrat1, Licznik2 = Licznik1)),
    ((union(MałyKwadrat2, [3, 6, 7, 10], MałyKwadrat3), Licznik3 is Licznik2 + 1) ; (MałyKwadrat3 = MałyKwadrat2, Licznik3 = Licznik2)),
    ((union(MałyKwadrat3, [8, 11, 12, 15], MałyKwadrat4), Licznik4 is Licznik3 + 1) ; (MałyKwadrat4 = MałyKwadrat3, Licznik4 = Licznik3)),
    ((union(MałyKwadrat4, [9, 12, 13, 16], MałyKwadrat5), Licznik5 is Licznik4 + 1) ; (MałyKwadrat5 = MałyKwadrat4, Licznik5 = Licznik4)),
    ((union(MałyKwadrat5, [10, 13, 14, 17], MałyKwadrat6), Licznik6 is Licznik5 + 1) ; (MałyKwadrat6 = MałyKwadrat5, Licznik6 = Licznik5)),
    ((union(MałyKwadrat6, [15, 18, 19, 22], MałyKwadrat7), Licznik7 is Licznik6 + 1) ; (MałyKwadrat7 = MałyKwadrat6, Licznik7 = Licznik6)),
    ((union(MałyKwadrat7, [16, 19, 20, 23], MałyKwadrat8), Licznik8 is Licznik7 + 1) ; (MałyKwadrat8 = MałyKwadrat7, Licznik8 = Licznik7)),
    ((union(MałyKwadrat8, [17, 20, 21, 24], Wynik), Licznik is Licznik8 + 1) ; (Wynik = MałyKwadrat8, Licznik = Licznik8)).

średnie_kwadraty(OtrzymanyWynik, Licznik, Wynik) :-
    Kwadrat1 = [1, 2, 4, 6, 11, 13, 15, 16],
    Kwadrat2 = [2, 3, 5, 7, 12, 14, 16, 17],
    Kwadrat3 = [8, 9, 11, 13, 18, 20, 22, 23],
    Kwadrat4 = [9, 10, 12, 14, 19, 21, 23, 24],
    ((union(OtrzymanyWynik, Kwadrat1, ŚredniKwadrat1), Licznik1 = 1) ; (ŚredniKwadrat1 = OtrzymanyWynik, Licznik1 = 0)),
    ((union(ŚredniKwadrat1, Kwadrat2, ŚredniKwadrat2), Licznik2 is Licznik1 + 1) ; (ŚredniKwadrat2 = ŚredniKwadrat1, Licznik2 = Licznik1)),
    ((union(ŚredniKwadrat2, Kwadrat3, ŚredniKwadrat3), Licznik3 is Licznik2 + 1) ; (ŚredniKwadrat3 = ŚredniKwadrat2, Licznik3 = Licznik2)),
    ((union(ŚredniKwadrat3, Kwadrat4, Wynik), Licznik is Licznik3 + 1) ; (Wynik = ŚredniKwadrat3, Licznik = Licznik3)).

duże_kwadraty(OtrzymanyWynik, Licznik, Wynik) :-
    Kwadrat1 = [1, 2, 3, 4, 7, 11, 14, 18, 21, 22, 23, 24],
    ((union(OtrzymanyWynik, Kwadrat1, Wynik), Licznik = 1) ; (Wynik = OtrzymanyWynik, Licznik = 0)).


zapałki(K, D, S, M) :-
    małe_kwadraty(M, Wynik1),
    średnie_kwadraty(Wynik1, S, Wynik2),
    duże_kwadraty(Wynik2, D, Wynik3),
    length(Wynik3, Długość),
    K is 24 - Długość,
    write("Rozwiązanie:\n"),
    narysuj_zapałki(Wynik3).

narysuj_zapałki(K) :-
    (member(1, K) -> write("+---+") ; write("+   +")),
    (member(2, K) -> write("---+") ; write("   +")),
    (member(3, K) -> write("---+\n") ; write("   +\n")),
    (member(4, K) -> write("|   ") ; write("    ")),
    (member(5, K) -> write("|   ") ; write("    ")),
    (member(6, K) -> write("|   ") ; write("    ")),
    (member(7, K) -> write("|\n") ; write(" \n")),
    (member(8, K) -> write("+---+") ; write("+   +")),
    (member(9, K) -> write("---+") ; write("   +")),
    (member(10, K) -> write("---+\n") ; write("   +\n")),
    (member(11, K) -> write("|   ") ; write("    ")),
    (member(12, K) -> write("|   ") ; write("    ")),
    (member(13, K) -> write("|   ") ; write("    ")),
    (member(14, K) -> write("|\n") ; write(" \n")),
    (member(15, K) -> write("+---+") ; write("+   +")),
    (member(16, K) -> write("---+") ; write("   +")),
    (member(17, K) -> write("---+\n") ; write("   +\n")),
    (member(18, K) -> write("|   ") ; write("    ")),
    (member(19, K) -> write("|   ") ; write("    ")),
    (member(20, K) -> write("|   ") ; write("    ")),
    (member(21, K) -> write("|\n") ; write(" \n")),
    (member(22, K) -> write("+---+") ; write("+   +")),
    (member(23, K) -> write("---+") ; write("   +")),
    (member(24, K) -> write("---+\n") ; write("   +\n")).
