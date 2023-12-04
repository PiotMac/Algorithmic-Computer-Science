#=
LISTA 4
TESTY NA BAZIE WYKRESÓW DLA FUNKCJI Z ZADANIA 3.
Autor: Piotr Maciejończyk, 268425
=#

include("interpolation.jl")
using .Interpolation
using Polynomials
using Plots

n = [5, 10, 15]

f1 = x -> sin(2.0 * x) + cos(2.0 * x - 1.0)
interval_a = -5.0
interval_b = 5.0

for i in n
    local x1 = collect(range(interval_a, interval_b, length=i))
    local  y1 = f1.(x1)

    local ilo = Interpolation.ilorazyRoznicowe(x1, y1)
    local a = Interpolation.naturalna(x1, ilo)
    
    # Create a function from coefficients
    f(x) = sum(c * x^(j-1) for (j, c) in enumerate(a))

    local xAxis = collect(range(interval_a + (interval_a / 50.0), interval_b + (interval_b / 50.0), length = 1000))

    local p = plot(size=(800,600))

    plot!(p, xAxis, f.(xAxis), label="Wielomian interpolacyjny")
    plot!(p, xAxis, f1.(xAxis), label="Interpolowana funkcja")
    scatter!(p, x1, y1, label="Węzły interpolacji")
    title!("Interpolacja Newtona")
    xlabel!("x")
    ylabel!("y")
    ylims!(-5.5, 5.5)

    savefig("TESTa_$i.png")
end

f1 = x -> cbrt(x) * exp(x)
interval_a = -1.0
interval_b = 0.2

for i in n
    local x1 = collect(range(interval_a, interval_b, length=i))
    local  y1 = f1.(x1)

    local ilo = Interpolation.ilorazyRoznicowe(x1, y1)
    local a = Interpolation.naturalna(x1, ilo)
    
    # Create a function from coefficients
    f(x) = sum(c * x^(j-1) for (j, c) in enumerate(a))

    local xAxis = collect(range(interval_a + (interval_a / 50.0), interval_b + (interval_b / 50.0), length = 1000))

    local p = plot(size=(800,600))

    plot!(p, xAxis, f.(xAxis), label="Wielomian interpolacyjny")
    plot!(p, xAxis, f1.(xAxis), label="Interpolowana funkcja")
    scatter!(p, x1, y1, label="Węzły interpolacji")
    title!("Interpolacja Newtona")
    xlabel!("x")
    ylabel!("y")
    #ylims!(-2.0, 3.0)

    savefig("TESTb_$i.png")
end

f1 = x -> 5.0*x^5 - 4.0*x^3 + 3.0*x^2 - 2.0*x + x - 1.0
interval_a = -1.2
interval_b = 1.1

for i in n
    local x1 = collect(range(interval_a, interval_b, length=i))
    local  y1 = f1.(x1)

    local ilo = Interpolation.ilorazyRoznicowe(x1, y1)
    local a = Interpolation.naturalna(x1, ilo)
    
    # Create a function from coefficients
    f(x) = sum(c * x^(j-1) for (j, c) in enumerate(a))

    local xAxis = collect(range(interval_a + (interval_a / 50.0), interval_b + (interval_b / 50.0), length = 1000))

    local p = plot(size=(800,600))

    plot!(p, xAxis, f.(xAxis), label="Wielomian interpolacyjny")
    plot!(p, xAxis, f1.(xAxis), label="Interpolowana funkcja")
    scatter!(p, x1, y1, label="Węzły interpolacji")
    title!("Interpolacja Newtona")
    xlabel!("x")
    ylabel!("y")
    #ylims!(-2.0, 3.0)

    savefig("TESTc_$i.png")
end
