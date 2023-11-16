#=
LISTA 2
ZADANIE 2
Autor: Piotr Maciejończyk, 268425
=#

using Plots

func(x) = exp(x) * log(1.0 + exp(-x))

# Create the plot
p = plot(func, -20.0, 40.0, label="e^x * ln(1 + e^(-x))", xlabel="x", ylabel="y", legend=:topleft)
hline!([1], color=:red, linestyle=:dash, label="limit when x approaches infinity")

display(p)
savefig(p, "/home/monek/Desktop/VSCode/Vsem/ON/L2/julia_vis_ex-2.png")

using SymPy

# Define the symbol variable
x = symbols("x")

# Define the symbolic expression
f_expr = exp(x) * log(1.0 + exp(-x))

# Calculate the limit
limit_result = limit(f_expr, x, Inf)

# Print the result
println(limit_result)
