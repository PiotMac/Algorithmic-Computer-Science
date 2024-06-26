:- module(prime_factors_file, [prime_factors/2]).

:- use_module(primes_file, [primes/2, is_prime/1]).

smallest_prime_factor(N, Factor) :-
    between(2, N, Factor),
    N mod Factor =:= 0,
    is_prime(Factor),
    !.

prime_factors(N, Factors) :-
    prime_factors_helper(N, Factors).

prime_factors_helper(1, []) :- !.
prime_factors_helper(N, [Factor | RestFactors]) :-
    smallest_prime_factor(N, Factor),
    Quotient is N // Factor,
    prime_factors_helper(Quotient, RestFactors).