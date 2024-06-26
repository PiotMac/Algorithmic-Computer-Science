:- module(primes_file, [primes/2, is_prime/1]).

is_prime(2).
is_prime(3).
is_prime(N) :-
    integer(N),
    N > 3,
    N mod 2 =\= 0,
    \+ has_factor(N, 3).

has_factor(N, Factor) :-
    N mod Factor =:= 0.

has_factor(N, Factor) :-
    Factor * Factor =< N,
    NextFactor is Factor + 2,
    has_factor(N, NextFactor).


primes(N, X) :-
    findall(Number, (between(2, N, Number), is_prime(Number)), X).