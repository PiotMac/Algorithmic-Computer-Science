#=
LISTA 5
FUNKCJE POMOCNICZE
Autor: Piotr Maciejończyk, 268425
=#

include("blocksys.jl")

module utilities
    export read_sparse_matrix, read_rhs_vector, write_solution_to_file, SparseMatrixMethods
    using SparseArrays
    using LinearAlgebra
    using Main.SpecificMatrix

    @enum SparseMatrixMethods begin
        GaussNoChoice
        GaussWithChoice
        LUNoChoice
        LUWithChoice
    end

    function read_sparse_matrix(file_path::String)
        file = open(file_path, "r")

        first_line = readline(file)
        first_values = split(first_line)
        n = parse(Int, first_values[1])
        l = parse(Int, first_values[2])

        # I_vector - vector of row indices
        # J_vector - vector of column indices
        # V_vector - vector of stored values
        I_vector = Vector{Int}()
        J_vector = Vector{Int}()
        V_vector = Vector{Float64}()

        for line in eachline(file)
            matrix_values = split(line)
            push!(I_vector, parse(Int, matrix_values[1]))
            push!(J_vector, parse(Int, matrix_values[2]))
            push!(V_vector, parse(Float64, matrix_values[3]))
        end
        A = sparse(I_vector, J_vector, V_vector)
        A = create_block_matrix(n, l, A)

        return A
    end

    function read_rhs_vector(file_path::String)
        file = open(file_path, "r")

        n = parse(Int, readline(file))
        b = Vector{Float64}()
        for line in eachline(file)
            push!(b, parse(Float64, line))
        end
        
        return n, b
    end

    function write_solution_to_file(solution::Vector{Float64}, solution_size::Int, used_method::Main.utilities.SparseMatrixMethods, was_rhs_read::Bool)
        base_file_path = "/home/monek/Desktop/VSCode/Vsem/ON/L5/solutions/"
        type = ""

        if used_method == Main.utilities.GaussWithChoice
            type = "ex1/choice/"
        elseif used_method == Main.utilities.GaussNoChoice
            type = "ex1/no_choice/"
        elseif used_method == Main.utilities.LUWithChoice
            type = "ex2/choice/"
        elseif used_method == Main.utilities.LUNoChoice
            type = "ex2/no_choice/"
        else
            #TODO
            type = "ex3/"
        end


        if was_rhs_read
            type = string(type, "known_rhs/")
        else
            type = string(type, "unknown_rhs/")
        end

        final_file_path = string(base_file_path, type, "solution", solution_size, ".txt")

        file = open(final_file_path, "w")

        if was_rhs_read

        else
            answer = ones(solution_size)
            error = norm(solution - answer) / norm(answer)
            println(file, error)
        end

        for i in 1:solution_size
            println(file, solution[i])
        end

        close(file)
    end
end
