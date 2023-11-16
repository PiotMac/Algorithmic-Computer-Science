#=
LISTA 2
ZADANIE 5
Autor: Piotr Maciejończyk, 268425
=#

# Float32
p = 0.01
# Truncated Float32
p_trunc = 0.01
# Float64
p64 = 0.01
# Const
r = 3

function equation(pn, r, fl)
    return fl(pn + fl(r * fl(pn * fl((1.0 - pn)))))
end

for i in 1:40
    p_new = equation(p, r, Float32)
    p_trunc_new = equation(p_trunc, r, Float32)
    p_64_new = equation(p64, r, Float64)
    if (i == 10)
        truncated_str = string(trunc(Int, p_trunc_new * 1000) / 1000)
        p_trunc_new = parse(Float32, truncated_str)
    end
    println("$i & $p_new & $p_trunc_new & $p_64_new\\\\ \n \\hline")
    global p = p_new
    global p_trunc = p_trunc_new
    global p64 = p_64_new
end
