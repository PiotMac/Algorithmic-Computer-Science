#=
LISTA 5
ZAD 1,2 i 3
Autor: Piotr Maciejończyk, 268425
=#

include("matrix_structure.jl")

module blocksys
    export gauss_without_main_element_choice, gauss_with_main_element_choice, create_b, 
    create_LU_from_matrix, create_LU_from_matrix_with_choice, default_LU_solver, choice_LU_solver
    using Main.SpecificMatrix

    function gauss_without_main_element_choice(A::SparseBlockMatrix, b::Vector{Float64})
        n = A.matrix_size
        l = A.block_size

        for k in 1:n-1
            bottom_index = (div(k, l) + 1) * l
            for i in k+1:bottom_index
                if A.matrix[k,k] == 0
                    error("Element 0 is on the diagonal of the matrix on the indices ($k, $k)!")
                end

                I = A.matrix[i, k] / A.matrix[k, k]
                A.matrix[i, k] = 0.0

                column_index = l + k
                if (column_index > n)
                    column_index = n
                end

                for j in k+1:column_index
                    A.matrix[i, j] -= I * A.matrix[k, j]
                end

                b[i] -= I * b[k]
            end
        end

        x = zeros(n)
        x[n] = b[n] / A.matrix[n, n]

        for k in n-1:-1:1
            difference = 0.0

            column_index = l + k
            if (column_index > n)
                column_index = n
            end

            for j in k+1:column_index
                difference += (A.matrix[k, j] * x[j])
            end
            x[k] = (b[k] - difference) / A.matrix[k, k]
        end

        return x
    end

    function gauss_with_main_element_choice(A::SparseBlockMatrix, b::Vector{Float64})
        n = A.matrix_size
        l = A.block_size
        rows_after_swap = [x for x in 1:n]

        for k in 1:n-1
            bottom_index = (div(k, l) + 1) * l
            index_found = k
            for i in k+1:bottom_index
                if (abs(A.matrix[rows_after_swap[i], k]) > abs(A.matrix[rows_after_swap[index_found], k]))
                    index_found = i
                end
            end

            if index_found != k
                rows_after_swap[k], rows_after_swap[index_found] = rows_after_swap[index_found], rows_after_swap[k]
            end

            for i in k+1:bottom_index
                if A.matrix[rows_after_swap[k], k] == 0
                    error("Element 0 is on the diagonal of the matrix on the indices ($k, $k)!")
                end

                I = A.matrix[rows_after_swap[i], k] / A.matrix[rows_after_swap[k], k]
                A.matrix[rows_after_swap[i], k] = 0.0

                # W przeciwieństwie do metody bez wybierania el. głównego, tutaj we wzorze na indeks kolumny
                # umieściłem "2 * l", zamiast jedynie "1 * l". Dzieje się tak, ponieważ takie przesunięcie indeksu może
                # wynieść maksymalnie wartość rozmiaru bloku.
                column_index = 2 * l + k
                if (column_index > n)
                    column_index = n
                end

                for j in k+1:column_index
                    A.matrix[rows_after_swap[i], j] -= I * A.matrix[rows_after_swap[k], j]
                end

                b[rows_after_swap[i]] -= I * b[rows_after_swap[k]]
            end
        end

        x = zeros(n)
        x[n] = b[rows_after_swap[n]] / A.matrix[rows_after_swap[n], n]

        for k in n-1:-1:1
            difference = 0.0

            column_index = 2 * l + k
            if (column_index > n)
                column_index = n
            end

            for j in k+1:column_index
                difference += (A.matrix[rows_after_swap[k], j] * x[j])
            end
            x[k] = (b[rows_after_swap[k]] - difference) / A.matrix[rows_after_swap[k], k]
        end

        return x
    end

    function create_LU_from_matrix(A::SparseBlockMatrix)
        n = A.matrix_size
        l = A.block_size
        for k in 1:n-1
            bottom_index = (div(k, l) + 1) * l
            for i in k+1:bottom_index
                if A.matrix[k,k] == 0
                    error("Element 0 is on the diagonal of the matrix on the indices ($k, $k)!")
                end

                I = A.matrix[i, k] / A.matrix[k, k]
                A.matrix[i, k] = I

                column_index = l + k
                if (column_index > n)
                    column_index = n
                end

                for j in k+1:column_index
                    A.matrix[i, j] -= I * A.matrix[k, j]
                end
            end
        end
    end

    function create_LU_from_matrix_with_choice(A::SparseBlockMatrix)
        n = A.matrix_size
        l = A.block_size
        rows_after_swap = [x for x in 1:n]

        for k in 1:n-1
            bottom_index = (div(k, l) + 1) * l
            index_found = k
            for i in k+1:bottom_index
                if (abs(A.matrix[rows_after_swap[i], k]) > abs(A.matrix[rows_after_swap[index_found], k]))
                    index_found = i
                end
            end

            if index_found != k
                rows_after_swap[k], rows_after_swap[index_found] = rows_after_swap[index_found], rows_after_swap[k]
            end

            for i in k+1:bottom_index
                if A.matrix[rows_after_swap[k], k] == 0
                    error("Element 0 is on the diagonal of the matrix on the indices ($k, $k)!")
                end

                I = A.matrix[rows_after_swap[i], k] / A.matrix[rows_after_swap[k], k]
                A.matrix[rows_after_swap[i], k] = I

                # W przeciwieństwie do metody bez wybierania el. głównego, tutaj we wzorze na indeks kolumny
                # umieściłem "2 * l", zamiast jedynie "1 * l". Dzieje się tak, ponieważ takie przesunięcie indeksu może
                # wynieść maksymalnie wartość rozmiaru bloku.
                column_index = 2 * l + k
                if (column_index > n)
                    column_index = n
                end

                for j in k+1:column_index
                    A.matrix[rows_after_swap[i], j] -= I * A.matrix[rows_after_swap[k], j]
                end
            end
        end

        return rows_after_swap
    end

    function default_LU_solver(LU::SparseBlockMatrix, b::Vector{Float64})
        n = LU.matrix_size
        l = LU.block_size

        # Solving Ly = b
        # (the diagonal in L consists of 1s)
        y = zeros(n)
        y[1] = b[1]
        for k in 2:n
            difference = 0.0
            left_index = max(1, div(k - 1, l) * l)

            for j in left_index:k-1
                difference += (LU.matrix[k, j] * y[j])
            end
            y[k] = b[k] - difference
        end

        # Solving Ux = y
        x = zeros(n)
        x[n] = y[n] / LU.matrix[n, n]

        for k in n-1:-1:1
            difference = 0.0

            column_index = l + k
            if (column_index > n)
                column_index = n
            end

            for j in k+1:column_index
                difference += (LU.matrix[k, j] * x[j])
            end
            x[k] = (y[k] - difference) / LU.matrix[k, k]
        end

        return x
    end

    function choice_LU_solver(LU::SparseBlockMatrix, b::Vector{Float64}, P::Vector{Int})
        n = LU.matrix_size
        l = LU.block_size

        # Solving Ly = Pb
        # (the diagonal in L consists of 1s)
        y = zeros(n)
        y[1] = b[P[1]]
        for k in 2:n
            difference = 0.0
            left_index = max(1, div(k - 1, l) * l - l)

            for j in left_index:k-1
                difference += (LU.matrix[P[k], j] * y[j])
            end
            y[k] = b[P[k]] - difference
        end

        # Solving Ux = y
        x = zeros(n)
        x[n] = y[n] / LU.matrix[P[n], n]

        for k in n-1:-1:1
            difference = 0.0

            # W przeciwieństwie do metody bez wybierania el. głównego, tutaj we wzorze na indeks kolumny
            # umieściłem "2 * l", zamiast jedynie "1 * l". Dzieje się tak, ponieważ takie przesunięcie indeksu może
            # wynieść maksymalnie wartość rozmiaru bloku.
            column_index = 2 * l + k
            if (column_index > n)
                column_index = n
            end

            for j in k+1:column_index
                difference += (LU.matrix[P[k], j] * x[j])
            end
            x[k] = (y[k] - difference) / LU.matrix[P[k], k]
        end

        return x
    end

    function create_b(A::SparseBlockMatrix)
        n = A.matrix_size
        l = A.block_size
        result = zeros(n)
        for row in 1:n
            column_index_start = 0
            if div(row - 1, l) == 0
                column_index_start = 1
            else
                column_index_start = div(row - 1, l) * l
            end
            
            column_index_end = l + row
            if column_index_end > n
                column_index_end = n
            end

            for column in column_index_start:column_index_end
                result[row] += A.matrix[row, column]
            end
        end

        return result
    end
end
