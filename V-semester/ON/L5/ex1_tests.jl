#=
LISTA 5
TESTY DO ZADANIA 1.
Autor: Piotr Maciejończyk, 268425
=#

include("utils.jl")
include("matrixgen.jl")

using .matrixgen
using .blocksys
using .utilities
using .SpecificMatrix
using UnicodePlots
using Test
using SparseArrays

function test_gauss_with_all_data_sets()
    # ------------------ TESTY DLA METODY ELIMINACJI GAUSSA BEZ WYBIERANIA ELEMENTU GŁÓWNEGO ------------------

    data_set_values = [16, 10000, 50000, 100000, 300000, 500000]
    # Test dla każdych danych testowych (przy czytaniu zarówno macierzy jak i wektora prawych stron)
    file_path = "/home/monek/Desktop/VSCode/Vsem/ON/L5/data/"
    for value in data_set_values
        matrix_file_path = string(file_path, "data", value, "/A.txt")
        rhs_file_path = string(file_path, "data", value, "/b.txt")

        A = read_sparse_matrix(matrix_file_path)
        n, b = read_rhs_vector(rhs_file_path)

        x = gauss_without_main_element_choice(A, b)
        @test isapprox(x, ones(n))

        write_solution_to_file(x, n, Main.utilities.GaussNoChoice, true)
    end
    # Test dla każdych danych testowych (wektor prawych stron jest obliczany)
    for value in data_set_values
        matrix_file_path = string(file_path, "data", value, "/A.txt")
        rhs_file_path = string(file_path, "data", value, "/b.txt")

        A = read_sparse_matrix(matrix_file_path)
        n, b_read = read_rhs_vector(rhs_file_path)
        b = create_b(A)
        @test isapprox(b, b_read)

        x = gauss_without_main_element_choice(A, b)
        @test isapprox(x, ones(n))

        write_solution_to_file(x, n, Main.utilities.GaussNoChoice, false)
    end
    # ------------------ TESTY DLA METODY ELIMINACJI GAUSSA WRAZ Z WYBOREM ELEMENTU GŁÓWNEGO ------------------

    # Test dla każdych danych testowych (przy czytaniu zarówno macierzy jak i wektora prawych stron)
    for value in data_set_values
        matrix_file_path = string(file_path, "data", value, "/A.txt")
        rhs_file_path = string(file_path, "data", value, "/b.txt")

        A = read_sparse_matrix(matrix_file_path)
        n, b = read_rhs_vector(rhs_file_path)

        x = gauss_with_main_element_choice(A, b)
        @test isapprox(x, ones(n))

        write_solution_to_file(x, n, Main.utilities.GaussWithChoice, true)
    end
    # Test dla każdych danych testowych (wektor prawych stron jest obliczany)
    for value in data_set_values
        matrix_file_path = string(file_path, "data", value, "/A.txt")
        rhs_file_path = string(file_path, "data", value, "/b.txt")

        A = read_sparse_matrix(matrix_file_path)
        n, b_read = read_rhs_vector(rhs_file_path)
        b = create_b(A)
        @test isapprox(b, b_read)

        x = gauss_with_main_element_choice(A, b)
        @test isapprox(x, ones(n))

        write_solution_to_file(x, n, Main.utilities.GaussWithChoice, false)
    end
end

function test_gauss_with_specific_examples()
    # ------------------ TESTY DLA METODY ELIMINACJI GAUSSA BEZ WYBIERANIA ELEMENTU GŁÓWNEGO ------------------
    # Macierz dobrze uwarunkowana
    test_file_path = "/home/monek/Desktop/VSCode/Vsem/ON/L5/tests/gauss_good_no_choice.txt"
    blockmat(10000, 10, 10.0, test_file_path)
    A = read_sparse_matrix(test_file_path)
    b = create_b(A)

    x = gauss_without_main_element_choice(A, b)
    @test isapprox(x, ones(10000))

    # Macierz źle uwarunkowana
    test_file_path = "/home/monek/Desktop/VSCode/Vsem/ON/L5/tests/gauss_bad_no_choice.txt"
    blockmat(10000, 10, 1000.0, test_file_path)
    A = read_sparse_matrix(test_file_path)
    b = create_b(A)

    x = gauss_without_main_element_choice(A, b)
    @test isapprox(x, ones(10000))
    # ------------------ TESTY DLA METODY ELIMINACJI GAUSSA WRAZ Z WYBOREM ELEMENTU GŁÓWNEGO ------------------
    # Macierz dobrze uwarunkowana
    test_file_path = "/home/monek/Desktop/VSCode/Vsem/ON/L5/tests/gauss_good_choice.txt"
    blockmat(10000, 10, 10.0, test_file_path)
    A = read_sparse_matrix(test_file_path)
    b = create_b(A)

    x = gauss_with_main_element_choice(A, b)
    @test isapprox(x, ones(10000))

    # Macierz źle uwarunkowana
    test_file_path = "/home/monek/Desktop/VSCode/Vsem/ON/L5/tests/gauss_bad_choice.txt"
    blockmat(10000, 10, 1000.0, test_file_path)
    A = read_sparse_matrix(test_file_path)
    b = create_b(A)

    x = gauss_with_main_element_choice(A, b)
    @test isapprox(x, ones(10000))
end

@testset "Gauss Elimination Tests" begin
    #test_gauss_with_all_data_sets()
    test_gauss_with_specific_examples()
end
