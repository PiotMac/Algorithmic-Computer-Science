#=
LISTA 1
ZADANIE 2
Autor: Piotr Maciejończyk, 268425
=#

# Kahan's method of calculating ϵ
# The equation: ϵ = 3(4/3 - 1) - 1
function kahan(T)
    macheps = convert(T, 0.0)
    macheps = T(T(3.0) * T(T(4.0 / 3.0) - 1.0) - 1.0)
    return macheps
end

# Performing the tests for each float type
for fl in (Float16, Float32, Float64)
    println(fl, ": ")
    println("Kahan's method: ", kahan(fl))
    println("Actual ϵ: ", eps(fl))
    println("--------------------------------------")
end

#=
For Float32 the result is positive.
For Float16 and Float64 the results
take form of -ϵ.
=#
