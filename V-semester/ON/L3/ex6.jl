#=
LISTA 3
ZADANIE 6
Autor: Piotr Maciejończyk, 268425
=#

include("root-finding-methods.jl")
using .RootFindingMethods

f1 = x -> exp(1.0 - x) - 1.0
pf1 = x -> -exp(1.0 - x)
f2 = x -> x * exp(-x)
pf2 = x -> -exp(-x) * (x - 1.0)

delta, epsilon = 10^(-5), 10^(-5)


println("Solutions found by Wolfram Alpha:")
println("For f1: r = 1.0")
println("For f2: r = 0.0")

println(" & Metoda bisekcji & Metoda Newtona & Metoda siecznych\\\\ \n \\hline")

r1bisekcja, v1bisekcja, it1bisekcja, err1bisekcja = RootFindingMethods.mbisekcji(f1, 0.0, 3.0, delta, epsilon)
r1newton, v1newton, it1newton, err1newton = RootFindingMethods.mstycznych(f1, pf1, 0.0, delta, epsilon, 100)
r1sieczne, v1sieczne, it1sieczne, err1sieczne = RootFindingMethods.msiecznych(f1, 0.0, 2.0, delta, epsilon, 100)

println("r & $r1bisekcja & $r1newton & $r1sieczne\\\\ \n \\hline")
println("v & $v1bisekcja & $v1newton & $v1sieczne\\\\ \n \\hline")
println("it & $it1bisekcja & $it1newton & $it1sieczne\\\\ \n \\hline")
println("err & $err1bisekcja & $err1newton & $err1sieczne\\\\ \n \\hline")

r2bisekcja, v2bisekcja, it2bisekcja, err2bisekcja = RootFindingMethods.mbisekcji(f2, -1.0, 5.0, delta, epsilon)
r2newton, v2newton, it2newton, err2newton = RootFindingMethods.mstycznych(f2, pf2, -4.0, delta, epsilon, 100)
r2sieczne, v2sieczne, it2sieczne, err2sieczne = RootFindingMethods.msiecznych(f2, -4.0, -2.5, delta, epsilon, 100)

println("r & $r2bisekcja & $r2newton & $r2sieczne\\\\ \n \\hline")
println("v & $v2bisekcja & $v2newton & $v2sieczne\\\\ \n \\hline")
println("it & $it2bisekcja & $it2newton & $it2sieczne\\\\ \n \\hline")
println("err & $err2bisekcja & $err2newton & $err2sieczne\\\\ \n \\hline")

# KOLEJNE ROZWIĄZANIA DLA METODY NEWTONA

println(" & \$f_{1}\$ & \$f_{2}\$\\\\ \n \\hline")
println("\$x_{0}\$ & r & v & it & err & r & v & it & err\\\\ \n \\hline")

r3newton, v3newton, it3newton, err3newton = RootFindingMethods.mstycznych(f1, pf1, 1.0, delta, epsilon, 100)
r4newton, v4newton, it4newton, err4newton = RootFindingMethods.mstycznych(f1, pf1, 5.0, delta, epsilon, 100)
r5newton, v5newton, it5newton, err5newton = RootFindingMethods.mstycznych(f1, pf1, 15.0, delta, epsilon, 100)

r6newton, v6newton, it6newton, err6newton = RootFindingMethods.mstycznych(f2, pf2, 0.0, delta, epsilon, 100)
r7newton, v7newton, it7newton, err7newton = RootFindingMethods.mstycznych(f2, pf2, 1.0, delta, epsilon, 100)
r8newton, v8newton, it8newton, err8newton = RootFindingMethods.mstycznych(f2, pf2, 15.0, delta, epsilon, 100)

println("0.0 & $r3newton & $v3newton & $it3newton & $err3newton & $r6newton & $v6newton & $it6newton & $err6newton\\\\ \n \\hline")
println("1.0 & $r4newton & $v4newton & $it4newton & $err4newton & $r7newton & $v7newton & $it7newton & $err7newton\\\\ \n \\hline")
println("15.0 & $r5newton & $v5newton & $it5newton & $err5newton & $r8newton & $v8newton & $it8newton & $err8newton\\\\ \n \\hline")
