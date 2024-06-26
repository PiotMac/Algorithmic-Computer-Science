:- use_module(merge_sort_file).
:- use_module(diophantine_file).
:- use_module(prime_factors_file).
:- use_module(totient_file).
:- use_module(primes_file).

test_everything :-
    writeln('########## TESTING MERGESORT ##########'),
    (
        % Test 1: Empty List
        merge_sort([], Sorted1),
        assertion(Sorted1 == []),
        writeln('Test 1 (Empty List): SUCCESS')
    ;
        writeln('Test 1 (Empty List): FAIL')
    ),

    (
        % Test 2: List with duplicates
        merge_sort([4, 4, 3, 2, 15, 21, 15, 8, 10], Sorted2),
        assertion(Sorted2 == [2, 3, 4, 4, 8, 10, 15, 15, 21]),
        writeln('Test 2 (List with duplicates): SUCCESS')
    ;
        writeln('Test 2 (List with duplicates): FAIL')
    ),

    (
        % Test 3: Sorted List
        merge_sort([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], Sorted3),
        assertion(Sorted3 == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
        writeln('Test 3 (Sorted List): SUCCESS')
    ;
        writeln('Test 3 (Sorted List): FAIL')
    ),

    (
        % Test 4: Reversed List
        merge_sort([10, 9, 8, 7, 6, 5, 4, 3, 2, 1], Sorted4),
        assertion(Sorted4 == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
        writeln('Test 4 (Reversed List): SUCCESS')
    ;
        writeln('Test 4 (Reversed List): FAIL')
    ),

    (
        % Test 5: Random List
        merge_sort([5, 2, 7, 3, 9, 1, 8, 4, 10, 5], Sorted5),
        assertion(Sorted5 == [1, 2, 3, 4, 5, 5, 7, 8, 9, 10]),
        writeln('Test 5 (Random List): SUCCESS')
    ;
        writeln('Test 5 (Random List): FAIL')
    ),

    writeln(''),
    writeln('########## TESTING DIOPHANTINE EQUATION ##########'),

    (
        % Test 1: Simple Case
        de(10, 15, X1, Y1, Z1),
        assertion(X1 == -1),
        assertion(Y1 == 1),
        assertion(Z1 == 5),
        writeln('Test 1 (Simple Case): SUCCESS')
    ;
        writeln('Test 1 (Simple Case): FAIL')
    ),

    (
        % Test 2: Large Numbers
        de(12345, 67890, X2, Y2, Z2),
        assertion(X2 == 11),
        assertion(Y2 == -2),
        assertion(Z2 == 15),
        writeln('Test 2 (Large Numbers): SUCCESS')
    ;
        writeln('Test 2 (Large Numbers): FAIL')
    ),

    (
        % Test 3: A or B is 0
        de(0, 5, X3, Y3, Z3),
        assertion(X3 == 0),
        assertion(Y3 == 1),
        assertion(Z3 == 5),
        writeln('Test 3 (A or B is 0): SUCCESS')
    ;
        writeln('Test 3 (A or B is 0): FAIL')
    ),

    writeln(''),
    writeln('########## TESTING PRIME FACTORS ##########'),

    (
        % Test 1: Empty list
        prime_factors(1, A1),
        assertion(A1 == []),
        writeln('Test 1 (Empty list): SUCCESS')
    ;
        writeln('Test 1 (Empty list): FAIL')
    ),

    (
        % Test 2: Prime number
        prime_factors(17, A2),
        assertion(A2 == [17]),
        writeln('Test 2 (Prime number): SUCCESS')
    ;
        writeln('Test 2 (Prime number: FAIL')
    ),

    (
        % Test 3: Composite number
        prime_factors(60, A3),
        assertion(A3 == [2, 2, 3, 5]),
        writeln('Test 3 (Composite number): SUCCESS')
    ;
        writeln('Test 3 (Composite number: FAIL')
    ),

    (
        % Test 4: Large composite number
        prime_factors(1234567890, A4),
        assertion(A4 == [2, 3, 3, 5, 3607, 3803]),
        writeln('Test 3 (Large composite number): SUCCESS')
    ;
        writeln('Test 3 (Large composite number: FAIL')
    ),

    writeln(''),
    writeln('########## TESTING TOTIENT FUNCTION ##########'),

    (
        % Test 1: Totient of number 1
        totient(1, T1),
        assertion(T1 == 1),
        writeln('Test 1 (Totient of number 1): SUCCESS')
    ;
        writeln('Test 1 (Totient of number 1): FAIL')
    ),

    (
        % Test 2: Small composite number
        totient(10, T2),
        assertion(T2 == 4),
        writeln('Test 2 (Small composite number): SUCCESS')
    ;
        writeln('Test 2 (Small composite number): FAIL')
    ),

    (
        % Test 3: Small prime number
        totient(7, T3),
        assertion(T3 == 6),
        writeln('Test 3 (Small prime number): SUCCESS')
    ;
        writeln('Test 3 (Small prime number): FAIL')
    ),

    (
        % Test 4: Big composite number
        totient(99, T4),
        assertion(T4 == 60),
        writeln('Test 4 (Big composite number): SUCCESS')
    ;
        writeln('Test 4 (Big composite number): FAIL')
    ),

    (
        % Test 5: Big prime number
        totient(1234577, T5),
        assertion(T5 == 1234576),
        writeln('Test 5 (Big prime number): SUCCESS')
    ;
        writeln('Test 5 (Big prime number): FAIL')
    ),

    writeln(''),
    writeln('########## TESTING PRIMES IN THE INTERVAL ##########'),

    (
        % Test 1: Testing number 2
        primes(2, P1),
        assertion(P1 == [2]),
        writeln('Test 1 (Testing number 2): SUCCESS')
    ;
        writeln('Test 1 (Testing number 2): FAIL')
    ),

    (
        % Test 2: Testing for N <= 60
        primes(60, P2),
        assertion(P2 == [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59]),
        writeln('Test 2 (Testing for N = 60): SUCCESS')
    ;
        writeln('Test 1 (Testing for N = 60): FAIL')
    ).