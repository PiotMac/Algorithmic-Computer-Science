#=
LISTA 3
TESTY DO ZADAŃ 1, 2, 3
Autor: Piotr Maciejończyk, 268425
=#

include("root-finding-methods.jl")
using .RootFindingMethods
using Test


# TESTY DLA METODY BISEKCJI (POŁOWIENIA)
function test_bisection_method()
    # Test dla prostej funkcji liniowej f(x) = x
    f(x) = x
    a, b = -10.0, 230.0
    delta, epsilon = 1e-3, 1e-3

    r, v, it, err = RootFindingMethods.mbisekcji(f, a, b, delta, epsilon)

    @test isapprox(r, 0.0, atol=delta)
    @test isapprox(v, 0.0, atol=epsilon)
    @test it > 0
    @test err == 0  # Sprawdź, czy metoda zakończyła działanie bez błędu


    # Test dla symetrycznego podziału
    f1(x) = x^2 - 4
    a1, b1 = 1.0, 3.0
    delta1, epsilon1 = 1e-5, 1e-5

    r1, v1, it1, err1 = RootFindingMethods.mbisekcji(f1, a1, b1, delta1, epsilon1)

    @test r1 == 2.0  # Sprawdź, czy wynik jest równy dokładnie 2.0
    @test v1 == 0.0  # Sprawdź, czy f(r) jest dokładnie równy 0.0
    @test it1 == 1  # Sprawdź, czy liczba iteracji jest równa 1
    @test err1 == 0  # Sprawdź, czy metoda zakończyła działanie bez błędu

    # Test dla funkcji, która nie zmienia znaku na przedziale [a, b]
    f2(x) = sin(x)
    a2, b2 = 0.5, 2.0
    delta2, epsilon2 = 1e-5, 1e-5

    r2, v2, it2, err2 = RootFindingMethods.mbisekcji(f2, a2, b2, delta2, epsilon2)

    @test r2 == a2
    @test v2 == f2(a2)
    @test it2 == 0  # Sprawdź, czy liczba iteracji jest równa 0
    @test err2 == 1  # Sprawdź, czy metoda zakończyła działanie z błędem

    # Test dla funkcji, która ma kilka pierwiastków w zadanym przedziale
    f3(x) = x^3 - 6x^2 + 11x - 6  # Pierwiastki tego wielomianu wynoszą: x=1, x=2, and x=3
    a3, b3 = 0.5, 3.5
    delta3, epsilon3 = 1e-5, 1e-5

    r3, v3, it3, err3 = RootFindingMethods.mbisekcji(f3, a3, b3, delta3, epsilon3)

    @test isapprox(r3, 1.0, atol=delta3) || isapprox(r3, 2.0, atol=delta3) || isapprox(r3, 3.0, atol=delta3)
    @test isapprox(v3, 0.0, atol=epsilon3)   # Sprawdż, czy f(r) jest bliski zeru
    @test it3 > 0  # Sprawdź, czy liczba iteracji jest większa od 0
    @test err3 == 0  # Sprawdż, czy metoda zakończyła działanie bez błędu

    # Test dla bardzo małego przedziału
    f4(x) = x^2 - 4
    a4, b4 = -2.00001, -1.99999
    delta4, epsilon4 = 1e-5, 1e-5

    r4, v4, it4, err4 = RootFindingMethods.mbisekcji(f4, a4, b4, delta4, epsilon4)

    @test isapprox(r4, -2.0, atol=delta4) 
    @test isapprox(v4, 0.0, atol=epsilon4) 
    @test it4 > 0
    @test err4 == 0

    # Test dla bardzo "stromej" funkcji
    f5(x) = sin(x) + x^3
    a5, b5 = -15.4214124, 7.412511111
    delta5, epsilon5 = 1e-5, 1e-5
    r5, v5, it5, err5 = RootFindingMethods.mbisekcji(f5, a5, b5, delta5, epsilon5)

    @test isapprox(r5, 0.0, atol=delta5) 
    @test isapprox(v5, 0.0, atol=epsilon5) 
    @test err5 == 0

    # Test dla bardzo "płaskiej" funkcji
    f6(x) = 0.01 * x
    a6, b6 = -2.4214124, 23.12415
    delta6, epsilon6 = 1e-5, 1e-5
    r6, v6, it6, err6 = RootFindingMethods.mbisekcji(f6, a6, b6, delta6, epsilon6)

    @test isapprox(r6, 0.0, atol=delta6) 
    @test isapprox(v6, 0.0, atol=epsilon6) 
    @test err6 == 0
end

function test_newtons_method()
    # Test dla funkcji kwadratowej
    f(x) = x^2 - 4.0
    df(x) = 2.0 * x
    delta, epsilon = 1e-5, 1e-5

    x0 = 3.0
    r, v, it, err = RootFindingMethods.mstycznych(f, df, x0, delta, epsilon, 100)

    @test isapprox(r, 2.0, atol=delta)
    @test isapprox(v, 0.0, atol=epsilon)
    @test it > 0
    @test err == 0

    # Test dla funkcji, która ma kilka pierwiastków w zadanym przedziale
    f1(x) = x^3 - 6x^2 + 11x - 6
    df1(x) = 3x^2 - 12x + 11

    x01 = 2.5
    r1, v1, it1, err1 = RootFindingMethods.mstycznych(f1, df1, x01, delta, epsilon, 100)

    @test isapprox(r1, 1.0, atol=delta) || isapprox(r1, 2.0, atol=delta) || isapprox(r1, 3.0, atol=delta)
    @test isapprox(v1, 0.0, atol=epsilon)
    @test it1 > 0 && it1 <= 100
    @test err1 == 0

    # Test dla funkcji, gdzie pochodna zbliża się do zera
    f2(x) = x^3 + 5
    df2(x) = 3.0 * x^2

    x02 = 0.0
    r2, v2, it2, err2 = RootFindingMethods.mstycznych(f2, df2, x02, delta, epsilon, 100)

    @test it2 > 0  && it2 <= 100
    @test err2 == 2

    # Test dla funkcji, gdzie przekroczono limit iteracji
    f3(x) = x^2 - 2
    df3(x) = 2x

    x03 = 1.0
    small_maxit = 2

    r3, v3, it3, err3 = RootFindingMethods.mstycznych(f3, df3, x03, delta, epsilon, small_maxit)
    @test !isapprox(r3, 0.0, atol=delta)
    @test !isapprox(v3, 0.0, atol=epsilon)
    @test it3 == small_maxit
    @test err3 == 1

    # Test, gdzie początkowe przybliżenie jest wynikiem dalekim od prawdziwego pierwiastka
    f4(x) = x^2 - 4
    df4(x) = 2x

    x04 = 50.0
    r4, v4, it4, err4 = RootFindingMethods.mstycznych(f4, df4, x04, delta, epsilon, 100)

    @test isapprox(r4, 2.0, atol=delta)
    @test isapprox(v4, 0.0, atol=epsilon)
    @test it4 > 0 && it4 <= 100
    @test err4 == 0

    # Test na podstawie slajdu z wykładu
    f5(x) = (0.5 * x)^2 + sin(x)
    df5(x) = 0.5 * x + cos(x)
    x05 = 1.5

    r5, v5, it5, err5 = RootFindingMethods.mstycznych(f5, df5, x05, delta, epsilon, 100)
    @test it5 == 4
    @test err5 == 0
end

function test_secant_method()
    # Test dla prostej funkcji liniowej
    f(x) = x
    x0, x1 = -2.0, 2.0
    delta, epsilon = 1e-5, 1e-5
    maxit = 100

    r, v, it, err = RootFindingMethods.msiecznych(f, x0, x1, delta, epsilon, maxit)
    @test isapprox(r, 0.0, atol=delta)
    @test isapprox(v, 0.0, atol=epsilon)
    @test it <= maxit
    @test err == 0

    # Test dla funkcji kwadratowej, której pierwiastki są znane
    f1(x) = x^2 - 4.0
    x0a, x1a = 1.0, 3.0


    r1, v1, it1, err1 = RootFindingMethods.msiecznych(f1, x0a, x1a, delta, epsilon, maxit)
    @test isapprox(r1, 2.0, atol=delta)
    @test isapprox(v1, 0.0, atol=epsilon)
    @test it1 <= maxit
    @test err1 == 0

    # Test dla funkcji sześciennej, krórej pierwiastki są znane
    f2(x) = x^3 - 27.0
    x0b, x1b = 2.0, 4.0
    delta, epsilon = 1e-5, 1e-5

    r2, v2, it2, err2 = RootFindingMethods.msiecznych(f2, x0b, x1b, delta, epsilon, maxit)
    @test isapprox(r2, 3.0, atol=delta)
    @test isapprox(v2, 0.0, atol=epsilon)
    @test it2 <= maxit
    @test err2 == 0

    # Test dla funkcji, która ma kilka pierwiastków w zadanym przedziale
    f3(x) = x^3 - 6x^2 + 11x - 6  # Pierwiastki tego wielomianu wynoszą: x=1, x=2, and x=3
    x0c, x1c = 0.5, 3.5

    r3, v3, it3, err3 = RootFindingMethods.msiecznych(f3, x0c, x1c, delta, epsilon, maxit)

    @test isapprox(r3, 1.0, atol=delta) || isapprox(r3, 2.0, atol=delta) || isapprox(r3, 3.0, atol=delta)
    @test isapprox(v3, 0.0, atol=epsilon)   # Sprawdż, czy f(r) jest bliski zeru
    @test it3 > 0 && it3 <= maxit # Sprawdź, czy liczba iteracji jest większa od 0
    @test err3 == 0  # Sprawdż, czy metoda zakończyła działanie bez błędu

    # Test dla funkcji, gdzie przekroczono limit iteracji
    f4(x) = x^2 - 2

    x0d, x1d = 1.0, 25.0
    small_maxit = 2

    r4, v4, it4, err4 = RootFindingMethods.msiecznych(f4, x0d, x1d, delta, epsilon, small_maxit)
    @test !isapprox(r4, 0.0, atol=delta)
    @test !isapprox(v4, 0.0, atol=epsilon)
    @test it4 == small_maxit
    @test err4 == 1
end

@testset "Root Finding Tests" begin
    test_bisection_method()
    test_newtons_method()
    test_secant_method()
end