count_inversions([], 0).
count_inversions([X|Xs], Count) :-
    findall(Y, (member(Y, Xs), Y < X), L),
    length(L, CurrentCount),
    count_inversions(Xs, RestCount),
    Count is CurrentCount + RestCount.

select_from_list(X, [X|Xs], Xs).
select_from_list(X, [Y|Ys], [Y|Zs]) :-
    select_from_list(X, Ys, Zs).

permutation_of_list([],[]).
permutation_of_list(Xs, [Z|Zs]) :-
    select_from_list(Z, Xs, Ys),
    permutation_of_list(Ys, Zs).

even_permutation(Xs, Ys) :-
    permutation_of_list(Xs, Ys),
    count_inversions(Ys, Count),
    Count mod 2 =:= 0.

odd_permutation(Xs, Ys) :-
    permutation_of_list(Xs, Ys),
    count_inversions(Ys, Count),
    Count mod 2 =:= 1.
