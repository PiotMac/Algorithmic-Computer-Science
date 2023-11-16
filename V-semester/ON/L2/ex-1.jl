#=
LISTA 2
ZADANIE 1
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

println(" & algorytm a) & algorytm b) & algorytm c) & algorytm d)\\\\ \n \\hline")

fl = Float32
x32_first = [fl(2.718281828), fl(-3.141592654), fl(1.414213562), fl(0.5772156649), fl(0.3010299957)]::Vector{Float32}
y32_first = [fl(1486.2497), fl(878366.9879), fl(-22.37492), fl(4773714.647), fl(0.000185049)]::Vector{Float32}

println("Float32 BEZ zmian & $(forward(x32_first, y32_first)) & $(backward(x32_first, y32_first)) & $(maxToMin(x32_first, y32_first)) & $(minToMax(x32_first, y32_first))\\\\ \n \\hline")

fl = Float32
x32 = [fl(2.718281828), fl(-3.141592654), fl(1.414213562), fl(0.577215664), fl(0.301029995)]::Vector{Float32}
y32 = [fl(1486.2497), fl(878366.9879), fl(-22.37492), fl(4773714.647), fl(0.000185049)]::Vector{Float32}

println("Float32 ZE zmianą & $(forward(x32, y32)) & $(backward(x32, y32)) & $(maxToMin(x32, y32)) & $(minToMax(x32, y32))\\\\ \n \\hline")

fl = Float64
x64_first = [fl(2.718281828), fl(-3.141592654), fl(1.414213562), fl(0.5772156649), fl(0.3010299957)]::Vector{Float64}
y64_first = [fl(1486.2497), fl(878366.9879), fl(-22.37492), fl(4773714.647), fl(0.000185049)]::Vector{Float64}

println("Float64 BEZ zmian & $(forward(x64_first, y64_first)) & $(backward(x64_first, y64_first)) & $(maxToMin(x64_first, y64_first)) & $(minToMax(x64_first, y64_first))\\\\ \n \\hline")

fl = Float64
x64 = [fl(2.718281828), fl(-3.141592654), fl(1.414213562), fl(0.577215664), fl(0.301029995)]::Vector{Float64}
y64 = [fl(1486.2497), fl(878366.9879), fl(-22.37492), fl(4773714.647), fl(0.000185049)]::Vector{Float64}

println("Float64 ZE zmianą & $(forward(x64, y64)) & $(backward(x64, y64)) & $(maxToMin(x64, y64)) & $(minToMax(x64, y64))\\\\ \n \\hline")
