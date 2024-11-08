:- use_module(library(clpfd)).

tasks([
    [2, 1, 3],
    [3, 2, 1],
    [4, 2, 2],
    [3, 3, 2],
    [3, 1, 1],
    [3, 4, 2],
    [5, 2, 1]
]).

resources(5, 5).

schedule(Horizon, Starts, MakeSpan) :-
    tasks(Tasks),
    resources(R1, R2),
    MakeSpan in 0..Horizon,
    mt(Tasks, Horizon, T1, T2, Starts, MakeSpan),
    cumulative(T1, [limit(R1)]),
    cumulative(T2, [limit(R2)]),
    once(labeling([min(MakeSpan), ff], [MakeSpan | Starts])).

mt([], _, [], [], [], _).
mt([[D, R1, R2] | L1], H, [task(S, D, E, R1, _) | L2], [task(S, D, E, R2, _) | L3], [S | L4], MakeSpan) :-
    S in 0..H,
    E #= S + D,
    MakeSpan #>= E,
    mt(L1, H, L2, L3, L4, MakeSpan).

