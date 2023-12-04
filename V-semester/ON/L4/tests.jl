#=
LISTA 4
TESTY DO ZADAŃ 1, 2, 3, 4
Autor: Piotr Maciejończyk, 268425
=#

include("interpolation.jl")
using .Interpolation
using Test
using Plots

# TESTY DLA FUNKCJI OBLICZAJĄCEJ ILORAZY RÓŻNICOWE
function test_difference_quotient_function()
    # Test dla wektorów różnej długości
    x = collect(1.0:1.0:100.0)
    y = collect(1.0:1.0:101.0)
    @test_throws DimensionMismatch Interpolation.ilorazyRoznicowe(x, y)

    # Test dla przykładu z wykładu
    x = [3.0, 1.0, 5.0, 6.0]
    y = [1.0, -3.0, 2.0, 4.0]
    expected_values = [1.0, 2.0, -3.0/8.0, 7.0/40.0]

    ilorazy_roznicowe = Interpolation.ilorazyRoznicowe(x, y)
    @test isapprox(ilorazy_roznicowe, expected_values)
end

# TESTY DLA FUNKCJI OBLICZAJĄCEJ WARTOŚĆ WIELOMIANU NEWTONA W PUNKCIE
function test_newton_polynomial_value()
    # Test dla wektorów różnej długości
    x = collect(1.0:1.0:100.0)
    y = collect(1.0:1.0:101.0)
    t = 5.0
    @test_throws DimensionMismatch Interpolation.warNewton(x, y, t)

    # Test dla dużego wielomianu
    x = collect(1.0:1.0:10.0)
    f = x -> x^8 + 6.0*x^7 - 2.0*x^6 + 4.0*x^5 - 8.0*x^4 - x^3 + 3.0*x^2 + 5.0*x - 15.0

    y = f.(x)
    expected_value = f(8.0)
    ilorazy_roznicowe = Interpolation.ilorazyRoznicowe(x, y)

    received_value = Interpolation.warNewton(x, ilorazy_roznicowe, 8.0)
    @test expected_value == received_value
end

# TESTY DLA FUNKCJI OBLICZAJĄCEJ WSPÓŁCZYNNIKI POSTACI NATURALNEJ WIELOMIANU NEWTONA
function test_natural_form_coefficients()
    # Test dla wektorów różnej długości
    x = collect(1.0:1.0:100.0)
    y = collect(1.0:1.0:101.0)

    @test_throws DimensionMismatch Interpolation.naturalna(x, y)

    # Test dla funkcji liniowej
    f = x -> 102.0*x - 71.421
    original_a = [-71.421, 102.0]
    x = [16.7, 51.2]
    y = f.(x)
    ilorazy_roznicowe = Interpolation.ilorazyRoznicowe(x, y)
    a = Interpolation.naturalna(x, ilorazy_roznicowe)
    @test isapprox(original_a, a, atol=1e-5)

    #Test dla funkcji kwadratowej
    f = x -> 50129.12*x^2 + 8371.1241*x - 1201.2888
    original_a = [-1201.2888, 8371.1241, 50129.12]
    x = [-38.2322, 7.98237, 123.12345]
    y = f.(x)
    ilorazy_roznicowe = Interpolation.ilorazyRoznicowe(x, y)
    a = Interpolation.naturalna(x, ilorazy_roznicowe)
    @test isapprox(original_a, a, atol=1e-5)

    # Test dla skomplikowanego wielomianu
    f = x -> 7142.1256666*x^4 - 0.12414214124*x^3 + 15.151515*x^2 - 10000000.01*x + 2137.21372137
    original_a = [2137.21372137, -10000000.01, 15.151515, -0.12414214124, 7142.1256666]
    x = collect(-1.5:1.0:2.5)
    y = f.(x)

    ilorazy_roznicowe = Interpolation.ilorazyRoznicowe(x, y)
    a = Interpolation.naturalna(x, ilorazy_roznicowe)
    @test isapprox(original_a, a, atol=1e-5)
end

@testset "Interpolation Tests" begin
    test_difference_quotient_function()
    test_newton_polynomial_value()
    test_natural_form_coefficients()
end
