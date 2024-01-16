#=
LISTA 5
PROGRAM DO DEBUGOWANIA
Autor: Piotr Maciejończyk, 268425
=#

include("utils.jl")

using UnicodePlots
using LinearAlgebra
using .blocksys
using .utilities
using .SpecificMatrix

using BenchmarkTools

matrix_file_path = "/home/monek/Desktop/VSCode/Vsem/ON/L5/data/data10000/A.txt"
rhs_file_path = "/home/monek/Desktop/VSCode/Vsem/ON/L5/data/data10000/b.txt"
A = read_sparse_matrix(matrix_file_path)
n, b_read = read_rhs_vector(rhs_file_path)
#b_calculated = create_b(A)
#result = @time gauss_without_main_element_choice(A, b_read)
#x = default_LU_solver(A, b_read)
#x = gauss_without_main_element_choice(A, b_calculated)
P = create_LU_from_matrix_with_choice(A)
x = choice_LU_solver(A, b_read, P)
print(x)
isapprox(ones(n), x)
#spy(A.matrix)
#spy(A.matrix[1:100, 1:100])
#x = choice_LU_solver(A, b_read, P)

#write_solution_to_file(x, n, Main.utilities.GaussNoChoice, false)
