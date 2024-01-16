#=
LISTA 5
WYKRESY
Autor: Piotr Maciejończyk, 268425
=#

include("utils.jl")
include("matrixgen.jl")

using .matrixgen
using .blocksys
using .utilities
using .SpecificMatrix
using BenchmarkTools
using Test
using SparseArrays
using Plots
using ColorTypes

function generate_tests()
    test_path = "/home/monek/Desktop/VSCode/Vsem/ON/L5/charts/testing_data/"
    data_set_values = [x for x in 10000:10000:100000]

    for size in data_set_values
        blockmat(size, 10, 1.0, string(test_path, "data", size, "/A.txt"))
    end
end

function test_block_sizes()
    data_set_values = [x for x in 1000:1000:10000]
    block_sizes = [2, 5, 10, 20, 25]

    timeSeries = []
    memorySeries = []
    for block_size in block_sizes
        times = []
        memories = []
        for size in data_set_values
            matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
            A = utilities.read_sparse_matrix("matrix_example.txt")
            b = blocksys.create_b(A)

            result = @timed blocksys.gauss_without_main_element_choice(A, b)
            #result = @timed blocksys.gauss_with_main_element_choice(A, b)

            #result1 = @timed blocksys.create_LU_from_matrix(A)
            #result2 = @timed blocksys.default_LU_solver(A, b)

            #result1 = @timed blocksys.create_LU_from_matrix_with_choice(A)
            #result2 = @timed blocksys.choice_LU_solver(A, b, result1.value)

            push!(times, result.time)
            #push!(times, result1.time + result2.time)
            push!(memories, result.bytes / 1024)
            #push!(memories, result1.bytes / 1024 + result2.bytes / 1024)
        end
        push!(timeSeries, times)
        push!(memorySeries, memories)
    end

    colors = distinguishable_colors(length(block_sizes))

    #for i in eachindex(block_sizes)
    #    plot!(data_set_values, timeSeries[i], label="block_size = $(block_sizes[i])", color=colors[i], linewidth = 2.0)
    #end

    for i in eachindex(block_sizes)
        plot!(data_set_values, memorySeries[i], label="block_size = $(block_sizes[i])", color=colors[i], linewidth = 2.0)
    end

    #title = "Złożoność czasowa w zależności od rozmiaru bloku"
    title = "Zużycie pamięci w zależności od rozmiaru bloku"
    title!(title)
    xlabel!("n")
    #ylabel!("czas [s]")
    ylabel!("pamięć [KB]")
    xticks!(data_set_values)
    #savefig("/home/monek/Desktop/VSCode/Vsem/ON/L5/charts/time/block_time_comparison_LU_with.png")
    savefig("/home/monek/Desktop/VSCode/Vsem/ON/L5/charts/memory/block_memory_comparison_gauss_without.png")
end

function test_time_complexity()
    data_set_values = [x for x in 10000:10000:100000]
    block_size = 5

    colors = distinguishable_colors(4)

    matrixgen.blockmat(10000, block_size, 1.0, "matrix_example.txt")
    test_A = utilities.read_sparse_matrix("matrix_example.txt")
    test_b = blocksys.create_b(test_A)

    test_result = @timed blocksys.gauss_with_main_element_choice(test_A, test_b)

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result = @timed blocksys.gauss_with_main_element_choice(A, b)

        push!(timeSeries, result.time)
    end

    plot!(data_set_values, timeSeries, label="Gauss z czesciowym wyborem", color=colors[1])

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result = @timed blocksys.gauss_without_main_element_choice(A, b)

        push!(timeSeries, result.time)
    end  

    plot!(data_set_values, timeSeries, label="Gauss bez wyboru", color=colors[2])

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result1 = @timed blocksys.create_LU_from_matrix(A)
        result2 = @timed blocksys.default_LU_solver(A, b)

        times = result1.time + result2.time

        push!(timeSeries, times)
    end 

    plot!(data_set_values, timeSeries, label="LU bez wyboru", color=colors[3])

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result1 = @timed blocksys.create_LU_from_matrix_with_choice(A)
        result2 = @timed blocksys.choice_LU_solver(A, b, result1.value)

        times = result1.time + result2.time

        push!(timeSeries, times)
    end 

    plot!(data_set_values, timeSeries, label="LU z czesciowym wyborem", color=colors[4])

    title = "Prędkość wykonywania się algorytmów"
    title!(title)
    xlabel!("n")
    ylabel!("czas [s]")
    xticks!(data_set_values)
    savefig("/home/monek/Desktop/VSCode/Vsem/ON/L5/charts/time/time_comparison.png")
end

function test_memory_usage()
    data_set_values = [x for x in 10000:10000:100000]
    block_size = 5
    colors = distinguishable_colors(4)

    matrixgen.blockmat(10000, block_size, 1.0, "matrix_example.txt")
    test_A = utilities.read_sparse_matrix("matrix_example.txt")
    test_b = blocksys.create_b(test_A)

    test_result = @timed blocksys.gauss_with_main_element_choice(test_A, test_b)

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result = @timed blocksys.gauss_with_main_element_choice(A, b)

        times = (result.bytes / 1024)
        push!(timeSeries, times)
    end

    plot!(data_set_values, timeSeries, label="Gauss z czesciowym wyborem", color=colors[1])

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result = @timed blocksys.gauss_without_main_element_choice(A, b)

        times = (result.bytes / 1024)
        push!(timeSeries, times)
    end
 

    plot!(data_set_values, timeSeries, label="Gauss bez wyboru", color=colors[2])

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result1 = @timed blocksys.create_LU_from_matrix(A)
        result2 = @timed blocksys.default_LU_solver(A, b)

        times = ((result1.bytes + result2.bytes) / 1024)
        push!(timeSeries, times)
    end


    plot!(data_set_values, timeSeries, label="LU bez wyboru", color=colors[3])

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result1 = @timed blocksys.create_LU_from_matrix_with_choice(A)
        result2 = @timed blocksys.choice_LU_solver(A, b, result1.value)

        times = ((result1.bytes + result2.bytes) / 1024)
        push!(timeSeries, times)
    end

    plot!(data_set_values, timeSeries, label="LU z czesciowym wyborem", color=colors[4])

    title = "Zużycie pamięci algorytmów"
    title!(title)
    xlabel!("n")
    ylabel!("pamięć [KB]")
    xticks!(data_set_values)
    savefig("/home/monek/Desktop/VSCode/Vsem/ON/L5/charts/memory/memory_comparison.png")
end

function compare_to_brute_force_time()
    data_set_values = [x for x in 1000:1000:10000]
    block_size = 5
    colors = distinguishable_colors(5)

    matrixgen.blockmat(10000, block_size, 1.0, "matrix_example.txt")
    test_A = utilities.read_sparse_matrix("matrix_example.txt")
    test_b = blocksys.create_b(test_A)

    test_result = @timed blocksys.gauss_with_main_element_choice(test_A, test_b)

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result = @timed blocksys.gauss_with_main_element_choice(A, b)

        push!(timeSeries, result.time)
    end

    plot!(data_set_values, timeSeries, label="Gauss z czesciowym wyborem", color=colors[1])

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result = @timed blocksys.gauss_without_main_element_choice(A, b)

        push!(timeSeries, result.time)
    end  

    plot!(data_set_values, timeSeries, label="Gauss bez wyboru", color=colors[2])

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result1 = @timed blocksys.create_LU_from_matrix(A)
        result2 = @timed blocksys.default_LU_solver(A, b)

        times = result1.time + result2.time

        push!(timeSeries, times)
    end 

    plot!(data_set_values, timeSeries, label="LU bez wyboru", color=colors[3])

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result1 = @timed blocksys.create_LU_from_matrix_with_choice(A)
        result2 = @timed blocksys.choice_LU_solver(A, b, result1.value)

        times = result1.time + result2.time

        push!(timeSeries, times)
    end 

    plot!(data_set_values, timeSeries, label="LU z czesciowym wyborem", color=colors[4])

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result = @timed Array(A.matrix) \ b

        push!(timeSeries, result.time)
    end 

    plot!(data_set_values, timeSeries, label="Julia: A \\\\ b", color=colors[5])

    title = "Prędkość wykonywania się algorytmów"
    title!(title)
    xlabel!("n")
    ylabel!("czas [s]")
    xticks!(data_set_values)
    savefig("/home/monek/Desktop/VSCode/Vsem/ON/L5/charts/time/ultimate_time_comparison.png")
end

function compare_to_brute_force_memory()
    data_set_values = [x for x in 1000:1000:10000]
    block_size = 5
    colors = distinguishable_colors(5)

    matrixgen.blockmat(10000, block_size, 1.0, "matrix_example.txt")
    test_A = utilities.read_sparse_matrix("matrix_example.txt")
    test_b = blocksys.create_b(test_A)

    test_result = @timed blocksys.gauss_with_main_element_choice(test_A, test_b)

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result = @timed blocksys.gauss_with_main_element_choice(A, b)

        times = (result.bytes / 1024)
        push!(timeSeries, times)
    end

    plot!(data_set_values, timeSeries, label="Gauss z czesciowym wyborem", color=colors[1])

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result = @timed blocksys.gauss_without_main_element_choice(A, b)

        times = (result.bytes / 1024)
        push!(timeSeries, times)
    end
 

    plot!(data_set_values, timeSeries, label="Gauss bez wyboru", color=colors[2])

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result1 = @timed blocksys.create_LU_from_matrix(A)
        result2 = @timed blocksys.default_LU_solver(A, b)

        times = ((result1.bytes + result2.bytes) / 1024)
        push!(timeSeries, times)
    end


    plot!(data_set_values, timeSeries, label="LU bez wyboru", color=colors[3])

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result1 = @timed blocksys.create_LU_from_matrix_with_choice(A)
        result2 = @timed blocksys.choice_LU_solver(A, b, result1.value)

        times = ((result1.bytes + result2.bytes) / 1024)
        push!(timeSeries, times)
    end

    plot!(data_set_values, timeSeries, label="LU z czesciowym wyborem", color=colors[4])

    timeSeries = []
    for size in data_set_values
        matrixgen.blockmat(size, block_size, 1.0, "matrix_example.txt")
        A = utilities.read_sparse_matrix("matrix_example.txt")
        b = blocksys.create_b(A)

        result = @timed Array(A.matrix) \ b

        times = result.bytes / 1024

        push!(timeSeries, times)
    end 

    plot!(data_set_values, timeSeries, label="Julia: A \\\\ b", color=colors[5])

    title = "Zużycie pamięci algorytmów"
    title!(title)
    xlabel!("n")
    ylabel!("pamięć [KB]")
    xticks!(data_set_values)
    savefig("/home/monek/Desktop/VSCode/Vsem/ON/L5/charts/memory/ultimate_memory_comparison.png")
end

@testset "Testing everything" begin
    #generate_tests()
    #test_block_sizes()
    #compare_to_brute_force_time()
    #compare_to_brute_force_memory()
    #test_time_complexity()
    test_memory_usage()
    #test_LU_with_specific_examples()
end
