using Plots

#=
f = x -> 3.0 * sin(x)

function visualize_bisection(f, a, b, delta, epsilon)
    p = plot(size=(800,600))
    x_vals = a:0.01:b
    y_vals = f.(x_vals)

    hline!(p, [0], color=:black, label="")

    plot!(p, x_vals, y_vals, label="f(x) = 3 ⋅ sin(x)", linewidth=3)

    a_n, b_n = a, b
    e = b - a
    for i in 1:5
        e /= 2
        c = a_n + e
        scatter!([a_n, b_n], [0, 0], label="", markersize=5, markercolor=:blue)
        annotate!([(a_n, 0.5, text("a$i", :left)), (b_n, 0.5, text("b$i", :right))])
        plot!(p, [c, c], [f(a_n), f(b_n)], label="Iteration $i", linewidth=3)
        if sign(f(a_n)) != sign(f(c))
            b_n = c
        else
            a_n = c
        end
    end


    scatter!([1, 4], [0, 0],label="", markersize=5, markercolor=:red)

    xlabel!("x")
    ylabel!("f(x) = 3 ⋅ sin(x)")

    title!("Bisection Method Visualization")
    display(p)
    #savefig(p, "bisection_vis.png")
end

visualize_bisection(f, 1.0, 4.0, 0.01, 0.01)
=#

#=
f = x -> 3.0 * sin(x)

function visualize_bisection_gif(f, a, b, delta, epsilon, maxit)
    p = plot(size=(800, 600))
    x_vals = a:0.01:b
    y_vals = f.(x_vals)
    hline!(p, [0], color=:black, label="")
    plot!(p, x_vals, y_vals, label="f(x) = 0.1 * x^2 - 1")


    a_n, b_n = a, b
    e = b - a
    scatter!([a_n, b_n], [0, 0], label="", markersize=5, markercolor=:red)

    anim = Animation()
    frame(anim)
    for i in 1:maxit
        e /= 2
        c = a_n + e

        # Mark interval [a_n, b_n] and label the midpoint c
        scatter!([a_n, b_n], [0, 0], label="", markersize=5, markercolor=:red)
        annotate!([(a_n, 0.5, text("a", :left)), (b_n, 0.5, text("b", :right))])

        plot!(p, [c, c], [f(a_n), f(b_n)], label="Iteration $i", linewidth=3)
        if sign(f(a_n)) != sign(f(c))
            b_n = c
        else
            a_n = c
        end

        frame(anim)
    end

    gif(anim, "bisection_visualization.gif", fps=0.5)
end

visualize_bisection_gif(f, 1.0, 4.0, 0.01, 0.01, 5)

=#
#=

f = x -> 0.1 * x^2 - 1
pf = x -> 0.2 * x

function visualize_newton(f, pf, x0, delta, epsilon, maxit)
    p = plot(size=(800,600))
    x_vals = -2.5:0.01:10.0
    y_vals = f.(x_vals)
    hline!(p, [0], color=:black, label="")

    plot!(p, x_vals, y_vals, label="f(x) = 0.1 * x^2 - 1")

    x_n = x0
    for i in 1:maxit
        v = f(x_n)
        derivative = pf(x_n)
        x_n1 = x_n - v / derivative
        plot!(p, [x_n, x_n], [v, 0], label="", linewidth=2)
        plot!(p, [x_n, x_n1], [v, 0], label="Iteration $i", linestyle=:dash)
        scatter!([x_n1], [0], label="", markersize=5, markercolor=:red)
        if abs(x_n1 - x_n) < delta || abs(v) < epsilon
            break
        end
        x_n = x_n1
    end

    scatter!(p, [x_n, x_n], [f(x_n),0], label="Root", markersize=5, markercolor=:green)
    title!("Newton's Method Visualization")
    xlabel!("x")
    ylabel!("f(x) = 0.1 ⋅ x^2 - 1")
    display(p)
    savefig(p, "newton_vis.png")
end

visualize_newton(f, pf, 0.5, 0.01, 0.01, 5)

=#
#=
f = x -> 0.1 * x^2 - 1
pf = x -> 0.2 * x

function visualize_newton_gif(f, pf, x0, delta, epsilon, maxit)
    p = plot(size=(800,600))
    x_vals = -2.5:0.01:10.0
    y_vals = f.(x_vals)
    hline!(p, [0], color=:black, label="")

    plot!(p, x_vals, y_vals, label="f(x) = 0.1 * x^2 - 1")

    x_n = x0
    scatter!([x0], [0], label="", markersize=5, markercolor=:red)

    anim = Animation()
    frame(anim)
    for i in 1:maxit
        v = f(x_n)
        derivative = pf(x_n)
        x_n1 = x_n - v / derivative

        # Mark iteration i
        plot!(p, [x_n, x_n], [v, 0], label="", linewidth=2)
        plot!(p, [x_n, x_n1], [v, 0], label="Iteration $i", linestyle=:dash)
        scatter!([x_n1], [0], label="", markersize=5, markercolor=:red)

        frame(anim)

        if abs(x_n1 - x_n) < delta || abs(v) < epsilon
            break
        end
        x_n = x_n1
    end

    scatter!(p, [x_n, x_n], [f(x_n),0], label="Root", markersize=5, markercolor=:green)
    title!("Newton's Method Visualization")
    xlabel!("x")
    ylabel!("f(x) = 0.1 ⋅ x^2 - 1")
    
    gif(anim, "newton_visualization.gif", fps=0.5)
end

visualize_newton_gif(f, pf, 0.5, 0.01, 0.01, 5)

=#
#=

f = x -> x^2 - 1

function visualize_secant(f, x0, x1, delta, epsilon, maxit)
    p = plot(size=(800,600))
    x_vals = -0.1:0.01:1.5
    y_vals = f.(x_vals)
    hline!(p, [0], color=:black, label="")

    plot!(p, x_vals, y_vals, label="f(x) = 0.1 * x^2 - 1", linewidth=2, color="blue")

    x_n, x_nm1 = x0, x1
    for i in 1:maxit
        # Create a linear function connecting the points (x_n, f(x_n)) and (x_nm1, f(x_nm1))
        line_y = x -> ((f(x_nm1) - f(x_n)) / (x_nm1 - x_n)) * (x - x_n) + f(x_n)
        plot!(p, x_vals, line_y.(x_vals), label="Secant Line $i", linestyle=:dash, linewidth=3, color="red")
        x_np1 = x_n - f(x_n) * (x_n - x_nm1) / (f(x_n) - f(x_nm1))
        x_nm1, x_n = x_n, x_np1
        if abs(f(x_n)) < epsilon
            break
        end
        vline!(p, [x_n], color=:red, label="", linestyle=:dash)
        # Plot linear function
        linear_x_vals = collect(-0.1:0.01:1.5)
        m = (f(x_n) - f(x_nm1)) / (x_n - x_nm1)
        b = f(x_n) - m * x_n
        linear_y_vals = m .* linear_x_vals .+ b
        plot!(p, linear_x_vals, linear_y_vals, label="", linestyle=:dash)
    end

    title!("Secant Method Visualization")
    xlabel!("x")
    ylabel!("f(x) = 0.1 ⋅ x^2 - 1")
    display(p)
    savefig(p, "secant_vis.png")
end

visualize_secant(f, 0.2, 0.7, 0.01, 0.01, 2)
=#
#=
f = x -> x^2 - 1

function visualize_secant_gif(f, x0, x1, delta, epsilon, maxit)
    p = plot(size=(800,600))
    title!("Secant Method Visualization")
    xlabel!("x")
    ylabel!("f(x) = x^2 - 1")
    ylims!(-1.1, 1.2)
    
    x_vals = -0.1:0.01:1.5
    y_vals = f.(x_vals)
    hline!(p, [0], color=:black, label="")

    plot!(p, x_vals, y_vals, label="f(x) = x^2 - 1", linewidth=2, color="blue")

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

visualize_secant_gif(f, 0.2, 0.7, 0.0001, 0.0001, 5)
=#