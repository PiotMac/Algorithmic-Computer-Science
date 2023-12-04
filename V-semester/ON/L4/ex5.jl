#=
LISTA 4
ZADANIE 5
Autor: Piotr Maciejończyk, 268425
=#

include("interpolation.jl")
using .Interpolation
using Plots

n = [5, 10, 15]

#-------------------PODPUNKT a)-------------------

f1 = x -> exp(x)

a1 = 0.0
b1 = 1.0

for i in n
    p = Interpolation.rysujNnfx(f1, a1, b1, i)
    savefig(p, "EX5a_$i.png")
end

#-------------------PODPUNKT b)-------------------

f2 = x -> x^2 * sin(x)

a2 = -1.0
b2 = 1.0

for i in n
    p = Interpolation.rysujNnfx(f2, a2, b2, i)
    savefig(p, "EX5b_$i.png")
end
