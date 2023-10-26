#=
LISTA 1
ZADANIE 1
Liczba MAX
Autor: Piotr Maciejończyk, 268425
=#

# Function floatmin(T) return the smallest positive
# normal number representable by the floating-point type T
println("Minimum value for Float32: ", floatmin(Float32))
println("Minimum value for Float64: ", floatmin(Float64))
println()

# A function calculating the largest positive
# normal number representable by the floating-point type T
function calculateFloatMax(T, bits)
    max = convert(T, 0.0)
    two = convert(T, 2.0)
    # Creating a mantissa full of 1s
    # for the given length of bits
    # provided for the mantissa
    for i in range(0, length=bits)
        x = convert(T, i)
        max += two^x
    end
    # Checking if increased 'max' is
    # already infinity
    while !isinf(two * max)
        max = two * max
    end

    return max
end

# Printing values returned by Julia and my own functions
println("Maximum value for Float16: ", floatmax(Float16))
println("Maximum value for Float32: ", floatmax(Float32))
println("Maximum value for Float64: ", floatmax(Float64))

println("Calculated maximum value for Float16: ", calculateFloatMax(Float16, 11))
println("Calculated maximum value for Float32: ", calculateFloatMax(Float32, 24))
println("Calculated maximum value for Float64: ", calculateFloatMax(Float64, 53))