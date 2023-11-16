#=
LISTA 2
ZADANIE 6
Program do wizualizacji iteracji graficznych
Autor: Piotr Maciejończyk, 268425
=#

using Plots

fl = Float64

function draw_line_between_points(p, x1, y1, x2, y2)
    plot!(p, [x1, x2], [y1, y2], color=:black, label="")
end

function create_graphic_iterations(x0, c, iterations, filename)
    y0 = 0.0
    x = -3.0:0.000001:3.0
    y = x 
    equation = [xi^2 + c for xi in x]

    p = plot(size=(800,600))
    plot!(p, x, y, label="y = x")
    plot!(p, x, equation, label="y = x^2 + $c")
    hline!(p, [0], linestyle=:dash, label="")
    vline!(p, [0], linestyle=:dash, label="")

    for i in 1:iterations
        # Convert x0 to an array for subtraction
        x0_array = [x0]
        # Find intersection point of x^2 + c and vertical line x0
        idx1 = argmin(abs.(x .- x0_array))
        y_intersect1 = equation[idx1]

        # Find intersection point of y and the first point's y
        idx2 = argmin(abs.(y .- y_intersect1))
        x_intersect2 = x[idx2]

        # Draw vertical line between (x0, y0) and (x0, y_intersect)
        draw_line_between_points(p, x0, y0, x0, y_intersect1)

        # Draw horizontal line between (x0, y_intersect) and (x_intersect, y_intersect)
        draw_line_between_points(p, x0, y_intersect1, x_intersect2, y_intersect1)

        # Update x0 to the x-coordinate of the vertical line
        x0 = x_intersect2
        # Also update y0 to the y-coordinate of the horizontal line
        y0 = y_intersect1
    end

    xlims!(-3, 3)
    ylims!(-3, 3)

    display(p)
    #savefig(p, filename)
end

chosen_cs = [fl(-2), fl(-2), fl(-2), fl(-2), fl(-1), fl(-1), fl(-1), fl(-1)]
chosen_x0s = [fl(1), fl(2), fl(1.99), fl(1.99999999999999), fl(1), fl(-1), fl(0.75), fl(0.25)]
chosen_iterations = 40
for (c, x0) in zip(chosen_cs, chosen_x0s)
    create_graphic_iterations(x0, c, chosen_iterations, "plot_c=$(c)_x0=$(x0).png")
end
