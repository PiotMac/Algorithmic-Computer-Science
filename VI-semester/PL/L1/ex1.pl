jest_matką(X) :- matka(X, _).
jest_ojcem(X) :- ojciec(X, _).
jest_synem(X) :- mężczyzna(X), rodzic(_, X).
siostra(X, Y) :- kobieta(X), rodzic(Z, X), rodzic(Z, Y), X \= Y.
dziadek(X, Y) :- mężczyzna(X), rodzic(X, A), rodzic(A, Y).
rodzeństwo(X, Y) :- rodzic(Z, X), rodzic(Z, Y), X \= Y.

