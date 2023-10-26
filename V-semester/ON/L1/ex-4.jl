#=
LISTA 1
ZADANIE 4
Autor: Piotr Maciejończyk, 268425
=#

fl = Float64
function findSmallest(fl)
    epsilon = fl(1.0)

    while (fl(1.0) + epsilon) > fl(1.0)
        epsilon /= fl(2.0)
    end
    epsilon = fl(2.0) * epsilon

    # I start from the smallest number > 1
    # in this interval.
    x = fl(1 + epsilon)
    # The loop goes on until x does no longer
    # satisfy the equation or x >= 2
    while fl(x * fl(1 / x)) == fl(1) && fl(x) <= fl(2.0)
        # Adding up step by step
        x = fl(x + epsilon)
    end
    return x
end

result = findSmallest(fl)
println(result)
