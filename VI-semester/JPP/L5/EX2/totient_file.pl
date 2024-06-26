:- module(totient_file, [totient/2]).

:- use_module(prime_factors_file, [prime_factors/2]).

% Remove duplicates from a list
remove_duplicates([], []).
remove_duplicates([H | T], [H | T1]) :-
    exclude(=(H), T, T2),
    remove_duplicates(T2, T1).

% Calculate the totient function for N
totient(1, 1) :- !.
totient(N, T) :-
    prime_factors(N, Factors),
    remove_duplicates(Factors, DistinctFactors),
    totient_formula(N, DistinctFactors, T),
    !.

% p1,p2,...pk --> czynniki pierwsze liczby n bez duplikatów
% phi(n) = n * ((p1 - 1) / p1) * ((p2 - 1) / p2) * ... * ((pk - 1) / pk)
totient_formula(N, [], N).
totient_formula(N, [P | Rest], T) :-
    N1 is N * (P - 1) // P,
    totient_formula(N1, Rest, T).