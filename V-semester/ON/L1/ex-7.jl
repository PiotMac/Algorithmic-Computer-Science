#=
LISTA 1
ZADANIE 7
Autor: Piotr Maciejończyk, 268425
=#

fl = Float64

# f(x) = sin(x) + cos(3x)
function f(x)
    return fl(sin(x) + cos(3.0*x))
end

# f'(x) = cos(x) - 3 * sin(3x)
function accurateDerivative(x)
    return fl(cos(x) - fl(3.0 * sin(fl(3.0*x))))
end

# f'(x₀) ≈ ᵮ'(x₀) = (f(x₀ + h) - f(x₀)) / h
function approximateDerivative(x, n)
    h = fl(2.0^(-n)) 
    return fl(fl(f(fl(x + h)) - f(x)) / h)
end

# |f'(x₀) - ᵮ'(x₀)|
function error(acc, approx)
    return abs(fl(acc - approx))
end

accurate = accurateDerivative(1)
space = 33
hspace = 28
println("h            1 + h                       approximation                   error                ")
println("------------------------------------------------------------------------------------------------")
for i in 0:54
    if (i==0)
        print("2^(",-i, ")        ")
    elseif(i < 10)
        print("2^(",-i, ")       ")
    else
        print("2^(",-i, ")      ")
    end
    approx = approximateDerivative(1, i)
    additional_space = space - length(string(approx))
    h_1 = fl(1) + fl(2.0^(-i))
    additional_hspace = hspace - length(string(h_1))
    print(h_1)
    for k in 1:additional_hspace
        print(" ")
    end
    print(approx)
    for j in 1:additional_space
        print(" ")
    end
    println(error(accurate, approx))
    println("------------------------------------------------------------------------------------------------")
end

# Creating a plot of error for x = 1
using Plots
xs = 0:54
ys = map(x -> abs(approximateDerivative(1,x) - accurate), xs)

bar(xs, ys, legend=false, yaxis=:log)

