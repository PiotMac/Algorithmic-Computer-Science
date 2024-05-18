from sympy import symbols, diff, solve
import numpy as np
import matplotlib.pyplot as plt


def equation1(x, y):
    return (x ** 2 + y ** 2 + 4 * y) ** 2 - 16 * (x ** 2 + y ** 2)


def equation2(x, y):
    return 2 * (x ** 2 + 9) * (y ** 2 - 16) + (x ** 2 - 9) ** 2 + (y ** 2 - 16) ** 2


def equation3(x, y):
    return 350 * (x ** 2) * (y ** 2) - 225 * (x ** 2 + y ** 2) + 144 * (x ** 4 + y ** 4) + 81


# Generate values for x and y
x_values = [np.linspace(-6, 6, 400), np.linspace(-8, 8, 400), np.linspace(-1.5, 1.5, 400)]
y_values = [np.linspace(-9, 2, 400), np.linspace(-5, 5, 400), np.linspace(-1.5, 1.5, 400)]

for i in range(len(x_values)):
    # Create a meshgrid from x and y values
    X, Y = np.meshgrid(x_values[i], y_values[i])

    # Calculate the equation values for each point in the meshgrid
    match i:
        case 0:
            Z = equation1(X, Y)
        case 1:
            Z = equation2(X, Y)
        case 2:
            Z = equation3(X, Y)
        case _:
            Z = equation1(X, Y)

    # Plot the solutions
    plt.figure(i)
    plt.axhline(0, color='black', linewidth=1.5)
    plt.axvline(0, color='black', linewidth=1.5)
    plt.contour(X, Y, Z, levels=[0], colors='r', linewidths=2.0)
    plt.xlabel('x')
    plt.ylabel('y')
    match i:
        case 0:
            plt.title(r'$(x^2 + y^2 + 4y)^2 - 16(x^2 + y^2) = 0$')
        case 1:
            plt.title(r'$2(x^2 + 9)(y^2 - 16) + (x^2 - 9)^2 + (y^2 - 16)^2 = 0$')
        case 2:
            plt.title(r'$350x^2 y^2 - 15^2 (x^2 + y^2) + 12^2 (x^4 + y^4) + 81 = 0$')
        case _:
            plt.title(r'$(x^2 + y^2 + 4y)^2 - 16(x^2 + y^2) = 0$')

    plt.grid(True)

    # Find and plot singular points
    x, y = symbols('x y')

    equationA = (x ** 2 + y ** 2 + 4 * y) ** 2 - 16 * (x ** 2 + y ** 2)
    equationB = 2 * (x ** 2 + 9) * (y ** 2 - 16) + (x ** 2 - 9) ** 2 + (y ** 2 - 16) ** 2
    equationC = 350 * (x ** 2) * (y ** 2) - 225 * (x ** 2 + y ** 2) + 144 * (x ** 4 + y ** 4) + 81

    match i:
        case 0:
            equation = equationA
        case 1:
            equation = equationB
        case 2:
            equation = equationC
        case _:
            equation = equationA

    partial_x = diff(equation, x)
    partial_y = diff(equation, y)

    singular_points = solve((partial_x, partial_y), (x, y))

    for point in singular_points:
        x_val, y_val = point
        if equation.subs({x: x_val, y: y_val}) == 0:
            plt.scatter(x_val, y_val, color='blue', s=100, label='Singular Points')
    plt.legend()
    plt.figure(i)
    plt.savefig(f'equation{i + 1}.png')
