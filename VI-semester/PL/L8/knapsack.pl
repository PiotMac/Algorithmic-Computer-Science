:- use_module(library(clpfd)).

plecak(Wartości, Wielkości, Pojemność, Zmienne):-
    length(Wartości,L),
    length(Zmienne,L),
    Zmienne ins 0..1,
    scalar_product(Wartości, Zmienne, #= ,Cel),
    scalar_product(Wielkości, Zmienne, #=<, Pojemność),
    once(labeling([max(Cel)], Zmienne)).
