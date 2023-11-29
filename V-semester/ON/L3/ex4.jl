#=
LISTA 3
ZADANIE 4
Autor: Piotr Maciejończyk, 268425
=#

include("root-finding-methods.jl")
using .RootFindingMethods

f = x -> sin(x) - (0.5 * x)^2
pf = x -> cos(x) - 0.5 * x
delta, epsilon = 0.5 * 10^(-5), 0.5 * 10^(-5)

println(" & Metoda bisekcji & Metoda Newtona & Metoda siecznych\\\\ \n \\hline")

rbisekcja, vbisekcja, itbisekcja, errbisekcja = RootFindingMethods.mbisekcji(f, 1.5, 2.0, delta, epsilon)
rnewton, vnewton, itnewton, errnewton = RootFindingMethods.mstycznych(f, pf, 1.5, delta, epsilon, 100)
rsieczne, vsieczne, itsieczne, errsieczne = RootFindingMethods.msiecznych(f, 1.0, 2.0, delta, epsilon, 100)

println("r & $rbisekcja & $rnewton & $rsieczne\\\\ \n \\hline")
println("v & $vbisekcja & $vnewton & $vsieczne\\\\ \n \\hline")
println("it & $itbisekcja & $itnewton & $itsieczne\\\\ \n \\hline")
println("err & $errbisekcja & $errnewton & $errsieczne\\\\ \n \\hline")
