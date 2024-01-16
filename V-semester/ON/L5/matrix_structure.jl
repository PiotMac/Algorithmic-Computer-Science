#=
LISTA 5
OBIEKT MACIERZY
Autor: Piotr Maciejończyk, 268425
=#

module SpecificMatrix
    export SparseBlockMatrix, create_block_matrix
    using SparseArrays

    mutable struct SparseBlockMatrix
        matrix::SparseMatrixCSC{Float64, Int}
        matrix_size::Int
        block_size::Int
    end

    function create_block_matrix(n::Int, l::Int, A::SparseMatrixCSC{Float64, Int})
        return SparseBlockMatrix(A, n, l)
    end
end
