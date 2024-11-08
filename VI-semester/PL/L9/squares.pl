:- use_module(library(clpfd)).

wstaw_kwadrat([],_,_,[],[]):-
    !.
wstaw_kwadrat([Roz|Reszta],Szerokosc,Wysokosc,[f(S1,Roz,W1,Roz)|Wst],[S1,W1|Wsp]):-
    S #= Szerokosc - Roz,
    W #= Wysokosc - Roz,
    S1 in 0..S,
    W1 in 0..W,
    wstaw_kwadrat(Reszta,Szerokosc,Wysokosc,Wst,Wsp).

kwadraty(Rozmiar, Szerokosc, Wysokosc, Wspolrzedne):-
    wstaw_kwadrat(Rozmiar,Szerokosc,Wysokosc,Z,Wspolrzedne),
    disjoint2(Z),
    label(Wspolrzedne).
