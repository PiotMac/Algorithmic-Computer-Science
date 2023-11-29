#=
LISTA 3
ZADANIE 5
Autor: Piotr Maciejończyk, 268425
=#

include("root-finding-methods.jl")
using .RootFindingMethods

f = x -> (3 * x - exp(x))
pf = x -> 3 - exp(x)
delta, epsilon = 10^(-4), 10^(-4)

println("Solutions found by Wolfram Alpha:")
println("1. r = 0.619061")
println("2. r = 1.51213")

println("Przedział & r & v & it & err\\\\ \n \\hline")

r1, v1, it1, err1 = RootFindingMethods.mbisekcji(f, 0.0, 1.0, delta, epsilon)

println("[0, 1] & $r1 & $v1 & $it1 & $err1 \\\\ \n \\hline")

r2, v2, it2, err2 = RootFindingMethods.mbisekcji(f, 1.0, 2.0, delta, epsilon)

println("[1, 2] & $r2 & $v2 & $it2 & $err2\\\\ \n \\hline")

