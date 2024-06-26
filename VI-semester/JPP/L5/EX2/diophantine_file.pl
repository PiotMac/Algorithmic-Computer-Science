:- module(diophantine_file, [de/5]).

gcd_ext(0, B, 0, 1, B) :- B =\= 0.

gcd_ext(A, B, X, Y, G) :-
    A =\= 0,
    B =\= 0,
    A1 is B mod A,
    gcd_ext(A1, A, X1, Y1, G),
    X is Y1 - (B // A) * X1,
    Y is X1.

de(A, B, X, Y, Z) :-
    once(gcd_ext(A, B, X, Y, Z)).