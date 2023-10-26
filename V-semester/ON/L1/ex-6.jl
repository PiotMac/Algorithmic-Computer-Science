#=
LISTA 1
ZADANIE 6
Autor: Piotr Maciejończyk, 268425
=#

fl = Float64

# Function f(x) = √(x² + 1) - 1
function f(x::fl)
    result = fl(0)
    result = fl(fl(sqrt(fl(fl(x^(2)) + 1))) - 1)
    return result
end

# Function g(x) = x² / (√(x² + 1) + 1)
function g(x::fl)
    result = fl(0)
    result = fl(fl(x^(2)) / fl(sqrt(fl(fl(x^(2)) + 1)) + 1))
    return result
end

space = 22
# Testing the results of both functions
# for x = 8⁻ⁿ, where n ϵ N \ {0}
println("x             f(x)                              g(x)                ")
println("-----------------------------------------------------------------------")
for i in 1:15
    exponent = -fl(i)
    if (i<10)
        print("8^(",-i, ")        ")
    else
        print("8^(",-i, ")       ")
    end
    f_result = f(fl(8.0^(exponent)))
    print(f_result)
    length_of_f = length(string(f_result))
    additional_spaces = space - length_of_f
    for j in 1:additional_spaces
        print(" ")
    end
    print("            ")
    println(g(fl(8.0^(exponent))))
    println("-----------------------------------------------------------------------")
end
