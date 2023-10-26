#=
LISTA 1
ZADANIE 1
Eta maszynowe
Autor: Piotr Maciejończyk, 268425
=#

# A function calculating η for given type T
function calculateMachineEta(T)
    eta = convert(T, 1.0)
    zero = convert(T, 0.0)
    two = convert(T, 2.0)
    count = 0

    # Getting the smallest number > 0.0
    while (eta + zero) != zero
        eta /= two
        count += 1
    end
    # Redoing the process but
    # this time repeating 
    # it one less time
    eta = convert(T, 1.0)
    while count != 1
        eta /= two
        count -= 1
    end
    return eta
end

# Printing η for each type
println("η for Float16: ", nextfloat(Float16(0.0)))
println("η for Float32: ", nextfloat(Float32(0.0)))
println("η for Float64: ", nextfloat(Float64(0.0)))

# Calculating iteratively machine eta for each type
float16Eta = calculateMachineEta(Float16)
float32Eta = calculateMachineEta(Float32)
float64Eta = calculateMachineEta(Float64)

println("Calculated η for Float16: ", float16Eta)
println("Calculated η for Float32: ", float32Eta)
println("Calculated η for Float64: ", float64Eta)