#=
LISTA 2
ZADANIE 6
Autor: Piotr Maciejończyk, 268425
=#

fl = Float64

function equation(x, c)
    return fl(x^(2.0) + fl(c))
end

# ----------------------------- c = -2 -----------------------------
c1 = fl(-2)

x1 = fl(1)
x2 = fl(2)
x3 = fl(1.99999999999999)

println("############################################### c = -2 ###############################################")

for i in 1:40
    x1_new = equation(x1, c1)
    x2_new = equation(x2, c1)
    x3_new = equation(x3, c1)
    println("$i & $x1_new & $x2_new & $x3_new\\\\ \n \\hline")
    global x1 = x1_new
    global x2 = x2_new
    global x3 = x3_new
end

# ----------------------------- c = -1 -----------------------------
c2 = fl(-1)

x4 = fl(1)
x5 = fl(-1)
x6 = fl(0.75)
x7 = fl(0.25)

println("############################################### c = -1 ###############################################")

for i in 1:40
    x4_new = equation(x4, c2)
    x5_new = equation(x5, c2)
    x6_new = equation(x6, c2)
    x7_new = equation(x7, c2)
    println("$i & $x4_new & $x5_new & $x6_new & $x7_new\\\\ \n \\hline")
    global x4 = x4_new
    global x5 = x5_new
    global x6 = x6_new
    global x7 = x7_new
end