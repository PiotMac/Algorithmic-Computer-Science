#=
LISTA 2
ZADANIE 3
Autor: Piotr Maciejończyk, 268425
=#

using LinearAlgebra
include("hilb.jl")
include("matcond.jl")


function solve_matrix(A, n)
    x = ones(Float64, n)
    b = A * x

    gauss_method = A \ b
    inverted_method = inv(A) * b

    return gauss_method, inverted_method
end

function error(accurate, received)
    return (norm(accurate - received) / norm(accurate))
end

println("------------------------------------- HILBERT'S MATRIX ----------------------------------")
for n in 1:30
    A = hilb(n)
    condition = cond(A)
    rank_A = rank(A)
    gauss_result, inverted_result = solve_matrix(A, n)
    gauss_error = error(ones(Float64, n), gauss_result)
    inverted_error = error(ones(Float64, n), inverted_result)

    println("$n & $condition & $rank_A & $gauss_error & $inverted_error\\\\ \n \\hline")
end

println("------------------------------------- RANDOM MATRIX -------------------------------------")

for n in [5, 10, 20]
    for c in [Float64(1), Float64(10), Float64(10^3), Float64(10^7), Float64(10^12), Float64(10^16)]
        A = matcond(n, c)
        condition = cond(A)
        rank_A = rank(A)
        gauss_result, inverted_result = solve_matrix(A, n)
        gauss_error = error(ones(Float64, n), gauss_result)
        inverted_error = error(ones(Float64, n), inverted_result)

        println("$n & $c & $condition & $rank_A & $gauss_error & $inverted_error\\\\ \n \\hline")
    end
end


