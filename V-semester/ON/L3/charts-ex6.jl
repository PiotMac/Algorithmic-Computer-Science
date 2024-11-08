using Plots

delta, epsilon = 10^-5, 10^-5


f1_bisection = x -> exp(1 - x) - 1
f2_bisection = x -> x * exp(-x)

function visualize_bisection_f1_gif(f, a, b, delta, epsilon, maxit)
    p = plot(size=(800,600))

    title!("Bisection Method Visualization")
    xlabel!("x")
    ylabel!("f(x) = e^(1 - x) - 1")
    xlims!(-0.1, 1.6)
    

    x_vals = -0.1:0.01:1.6
    y_vals = f.(x_vals)
    hline!(p, [0], color=:black, label="")

    plot!(p, x_vals, y_vals, label="f(x) = e^(1 - x) - 1", linewidth=2)

    a_n, b_n = a, b

    anim = Animation()
    frame(anim)
    for i in 1:maxit
        e = (b_n - a_n) / 2
        c = a_n + e

        # Highlight the current interval
        vline!(p, [a_n, b_n], color=:gray, label="", linewidth=2)

        # Plot the iteration
        vline!(p, [c, c], label="Iteration $i", linewidth=2, color=:red)

        if sign(f(a_n)) != sign(f(c))
            b_n = c
        else
            a_n = c
        end

        frame(anim)
    end
    
    gif(anim, "bisection_visualization_f1.gif", fps=0.5)
end

function visualize_bisection_f2_gif(f, a, b, delta, epsilon, maxit)
    p = plot(size=(800,600))

    title!("Bisection Method Visualization")
    xlabel!("x")
    ylabel!("f(x) = x * e^(-x)")
    xlims!(-1.0, 0.5)

    x_vals = -1.0:0.01:0.5
    y_vals = f.(x_vals)
    hline!(p, [0], color=:black, label="")

    plot!(p, x_vals, y_vals, label="f(x) = x * e^(-x)", linewidth=2)

    a_n, b_n = a, b

    anim = Animation()
    frame(anim)
    for i in 1:maxit
        e = (b_n - a_n) / 2
        c = a_n + e

        # Highlight the current interval
        vline!(p, [a_n, b_n], color=:gray, label="", linewidth=2)

        # Plot the iteration
        vline!(p, [c, c], label="Iteration $i", linewidth=2, color=:red)

        if sign(f(a_n)) != sign(f(c))
            b_n = c
        else
            a_n = c
        end

        frame(anim)
    end
    
    gif(anim, "bisection_visualization_f2.gif", fps=0.5)
end

visualize_bisection_f1_gif(f1_bisection, 0.0, 1.5, delta, epsilon, 5)
visualize_bisection_f2_gif(f2_bisection, -1.0, 0.5, delta, epsilon, 5)


#=
f_newton = x -> 3.0 * x - exp(x)
pf_newton = x -> 3.0 - exp(x)

function visualize_newton_gif(f, pf, x0, delta, epsilon, maxit)
    p = plot(size=(800,600))
    title!("Newton's Method Visualization")
    xlabel!("x")
    ylabel!("f(x) = 3.0 * x - exp(x)")
    ylims!(-0.5, 0.3)
    xlims!(0.0, 2.0)
    x_vals = 0.0:0.01:2.0
    y_vals = f.(x_vals)
    hline!(p, [0], color=:black, label="")


    plot!(p, x_vals, y_vals, label="f(x) = 3.0 * x - exp(x)", linewidth=2)

    x_n = x0
    scatter!([x0], [0], label="", markersize=5, markercolor=:red)

    anim = Animation()
    frame(anim)
    for i in 1:maxit
        v = f(x_n)
        derivative = pf(x_n)
        x_n1 = x_n - v / derivative

        # Plot the iteration
        plot!(p, [x_n, x_n], [v, 0], label="", linewidth=2, color=:blue)
        plot!(p, [x_n, x_n1], [v, 0], label="Iteration $i", linewidth=3, color=:black)
        scatter!([x_n1], [0], label="", markersize=5, markercolor=:red)

        if abs(x_n1 - x_n) < delta || abs(v) < epsilon
            break
        end

        frame(anim)

        x_n = x_n1
    end
    scatter!(p, [x_n, x_n], [f(x_n), 0], label="Root", markersize=5, markercolor=:green)
    
    gif(anim, "newton_visualization.gif", fps=0.5)
end

visualize_newton_gif(f_newton, pf_newton, 1.3, delta, epsilon, 10)
=#
#=
f_secant = x -> abs(3.0 * x - exp(x))

function visualize_secant_gif(f, x0, x1, delta, epsilon, maxit)
    p = plot(size=(800,600))

    title!("Secant Method Visualization")
    xlabel!("x")
    ylabel!("f(x) = 3.0 * x - exp(x)")
    xlims!(0.0, 2.0)
    ylims!(-0.5, 0.3)

    x_vals = 0.0:0.01:2.0
    y_vals = f.(x_vals)
    hline!(p, [0], color=:black, label="")

    plot!(p, x_vals, y_vals, label="f(x) = 3.0 * x - exp(x)", linewidth=2, color=:blue)

    x_n, x_nm1 = x0, x1

    anim = Animation()
    frame(anim)
    for i in 1:maxit
        # Create a linear function connecting the points (x_n, f(x_n)) and (x_nm1, f(x_nm1))
        line_y = x -> ((f(x_nm1) - f(x_n)) / (x_nm1 - x_n)) * (x - x_n) + f(x_n)
        plot!(p, x_vals, line_y.(x_vals), label="Secant Line $i", linewidth=3, color="red")

        x_np1 = x_n - f(x_n) * (x_n - x_nm1) / (f(x_n) - f(x_nm1))
        x_nm1, x_n = x_n, x_np1

        if abs(f(x_n)) < epsilon
            break
        end

        # Plot vertical line
        vline!(p, [x_n], color=:red, label="", linestyle=:dash)

        frame(anim)
    end
    
    gif(anim, "secant_visualization.gif", fps=0.5)
end

visualize_secant_gif(f_secant, 1.0, 2.0, delta, epsilon, 10)
=#