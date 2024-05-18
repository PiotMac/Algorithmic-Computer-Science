% Gramatyka metamorficzna dla języka a^n b^n, gdzie n >= 0
first --> [].
first --> [a], first, [b].

% Gramatyka metamorficzna dla języka a^n b^n c^n, gdzie n >= 0
second --> [].
second --> abcBlocks.

abcBlocks --> a_seq(N), b_seq(N), c_seq(N).

a_seq(0) --> [].
a_seq(succ(N)) --> [a], a_seq(N).

b_seq(0) --> [].
b_seq(succ(N)) --> [b], b_seq(N).

c_seq(0) --> [].
c_seq(succ(N)) --> [c], c_seq(N).

% Gramatyka metamorficzna dla języka a^n b^(fib(n)), gdzie n >= 0
third --> [].
third --> a_seq(N), { fib(N, FibN) }, b_seq(FibN).


fib(0, 0).
fib(succ(0), succ(0)).
fib(succ(succ(N)), FibN) :-
    fib(succ(N), FibN1),
    fib(N, FibN2),
    peano_add(FibN1, FibN2, FibN).

peano_add(0, N, N).
peano_add(succ(M), N, succ(R)) :-
    peano_add(M, N, R).
