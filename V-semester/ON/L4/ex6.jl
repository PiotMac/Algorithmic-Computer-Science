#=
LISTA 4
ZADANIE 6
Autor: Piotr Maciejończyk, 268425
=#

include("interpolation.jl")
using .Interpolation
using Plots

n = [5, 10, 15]

#-------------------PODPUNKT a)-------------------

f1 = x -> abs(x)

a1 = -1.0
b1 = 1.0

for i in n
    p = Interpolation.rysujNnfx(f1, a1, b1, i)
    savefig(p, "EX6a_$i.png")
end

#-------------------PODPUNKT b)-------------------

f2 = x -> 1.0 / (1.0 + x^2)

a2 = -5.0
b2 = 5.0

for i in n
    p = Interpolation.rysujNnfx(f2, a2, b2, i)
    savefig(p, "EX6b_$i.png")
end
