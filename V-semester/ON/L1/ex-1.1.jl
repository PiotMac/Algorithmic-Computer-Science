#=
LISTA 1
ZADANIE 1
Epsilon maszynowe
Autor: Piotr Maciejończyk, 268425
=#

# Printing accurate machine epsilons for floats
print("ϵ for Float16: ")
println(eps(Float16))
print("ϵ for Float32: ")
println(eps(Float32))
print("ϵ for Float64: ")
println(eps(Float64))

# A function calculating ϵ for given type T
function calculateMachineEpsilon(T)
    # Converting into given type (ϵ starts from 1)
    epsilon = convert(T, 1.0)
    one = convert(T, 1.0)
    two = convert(T, 2.0)
    # Waiting until this equation is false
    while (one + epsilon) > one
        epsilon /= two
    end
    # We return the ϵ multiplied by 2
    # Because the machine epsilon "passes through"
    # The while loop
    return T(2.0) * epsilon
end

# Printing calculated ϵ
float16Eps = calculateMachineEpsilon(Float16)
float32Eps = calculateMachineEpsilon(Float32)
float64Eps = calculateMachineEpsilon(Float64)

println("Calculated ϵ for Float16: ", float16Eps)
println("Calculated ϵ for Float32: ", float32Eps)
println("Calculated ϵ for Float64: ", float64Eps)
println()

# W pliku nagłówkowym float.h w języku c podane są wartości
# dla maszynowego epsilona typów: float, double oraz long double.
# FLT_EPSILON = 1.192093e-07
# DBL_EPSILON = 2.22045e-16
# LDBL_EPSILON = 1.0842e-19
# Wartości maszynowego epsilon wyliczone dla float32(float)
# oraz float64(double) zgadzają się z tymi podanymi we <float.h>.