from sympy import symbols, simplify
import numpy as np
import matplotlib.pyplot as plt

t, x, y = symbols('t x y')

x_t = (1 - t**2) / (1 + t**2)
y_t = 2 * t / (1 + t**2)

first = x_t**2
second = y_t**2

implicit = simplify(first + second)

print(f"x^2 + y^2 = {implicit}")


def calculate_x(T):
    return (1.0 - T**2) / (1.0 + T**2)


def calculate_y(T):
    return (2.0 * T) / (1.0 + T**2)


t = np.linspace(-2 * np.pi, 2 * np.pi, 1000)
xs = calculate_x(t)
ys = calculate_y(t)

plt.figure(figsize=(6, 6))
plt.plot(xs, ys, "r", linewidth=2)
plt.title("Unit Circle")
plt.xlabel("x")
plt.ylabel("y")
plt.xlim(-2, 2)
plt.ylim(-2, 2)
plt.axhline(0, color = 'black', linewidth = 1)
plt.axvline(0, color = 'black', linewidth = 1)
plt.grid(True)
plt.savefig("implicit_circle.png")
