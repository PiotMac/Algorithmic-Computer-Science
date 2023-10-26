#=
LISTA 1
ZADANIE 5
Autor: Piotr Maciejończyk, 268425
=#

# a) an algorithm "forward"
function forward(v, u)
    # Checking if length(v) = length(u)
    # && >= 0
    S = fl(0)
    nX = length(v)
    nY = length(u)
    if (nX != nY || nX <= 0 || nY <= 0)
        return -1
    end
    n = nX
    # Iterating from first to last index
    for i in 1:n
        S = fl(S + fl(v[i] * u[i]))
    end
    return S
end

# b) an algorithm "backward"
function backward(v, u)
    # Checking if length(v) = length(u)
    # && >= 0
    S = fl(0)
    nX = length(v)
    nY = length(u)
    if (nX != nY || nX <= 0 || nY <= 0)
        return -1
    end
    n = nX
    # Iterating from last to first index
    for i in n:-1:1
        S = fl(S + fl(v[i] * u[i]))
    end
    return S
end

# c) - an algorithm from max to min
function maxToMin(v, u)
    # Checking if length(v) = length(u)
    # && >= 0
    S = fl(0)
    nX = length(v)
    nY = length(u)
    if (nX != nY || nX <= 0 || nY <= 0)
        return -1
    end
    n = nX

    result = []
    # Multplying values at each index and
    # pushing the results into a new vector
    for i in 1:n
        push!(result, fl(v[i] * u[i]))
    end

    positiveSum = fl(0)
    negativeSum = fl(0)
    # Creating a partial sum with non-negative numbers and reversing the order
    positiveSum = sum(sort(filter(a -> a >= 0, result), rev=true))
    # Creating a partial sum with negative numbers
    negativeSum = sum(sort(filter(a -> a < 0, result)))

    # Adding partial sums
    S = fl(positiveSum + negativeSum)

    return S
end

# d) - an algorithm from min to max
function minToMax(v, u)
    # Checking if length(v) = length(u)
    # && >= 0
    S = fl(0)
    nX = length(v)
    nY = length(u)
    if (nX != nY || nX <= 0 || nY <= 0)
        return -1
    end
    n = nX

    result = []
    # Multplying values at each index and
    # pushing the results into a new vector
    for i in 1:n
        push!(result, fl(v[i] * u[i]))
    end

    positiveSum = fl(0)
    negativeSum = fl(0)
    # Creating a partial sum with non-negative numbers
    positiveSum = sum(sort(filter(a -> a >= 0, result)))
    # Creating a partial sum with negative numbers and reversing the order
    negativeSum = sum(sort(filter(a -> a < 0, result), rev=true))

    # Adding partial sums
    S = fl(positiveSum + negativeSum)

    return S
end

fl = Float32
x32 = [fl(2.718281828), fl(-3.141592654), fl(1.414213562), fl(0.5772156649), fl(0.3010299957)]::Vector{Float32}
y32 = [fl(1486.2497), fl(878366.9879), fl(-22.37492), fl(4773714.647), fl(0.000185049)]::Vector{Float32}
answer = -1.00657107000000 * 10^(-11)

println("Float32: ")
println("Good answer: ", answer)
println("a) ", forward(x32, y32))
println("b) ", backward(x32, y32))
println("c) ", maxToMin(x32, y32))
println("d) ", minToMax(x32, y32))
println("-------------------")

fl = Float64
x64 = [fl(2.718281828), fl(-3.141592654), fl(1.414213562), fl(0.5772156649), fl(0.3010299957)]::Vector{Float64}
y64 = [fl(1486.2497), fl(878366.9879), fl(-22.37492), fl(4773714.647), fl(0.000185049)]::Vector{Float64}

println("Float64: ")
println("Good answer: ", answer)
println("a) ", forward(x64, y64))
println("b) ", backward(x64, y64))
println("c) ", maxToMin(x64, y64))
println("d) ", minToMax(x64, y64))
println("-------------------")
