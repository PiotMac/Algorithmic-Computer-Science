from sympy import symbols, groebner
import numpy as np
import matplotlib.pyplot as plt

x, y, a = symbols('x y a')

equation = (x - 1)*(x**2 + y**2) - a * x**2

# Step 3: Compute the Gröbner basis
G = groebner([equation], x, y, a, order='lex')

# Print the Gröbner basis
for g in G:
    print(g)


def plot_conchoid_of_de_sluze(ax, a, theta_max = np.pi * 2, num_points = 1000):
    theta = np.linspace(0, theta_max, num_points)
    r = (1.0 / np.cos(theta)) + (a * np.cos(theta))

    x = r * np.cos(theta)
    y = r * np.sin(theta)

    ax.plot(x, y, label=f'a = {a}')


a_values = [-4, -2, 0, 1, 2, 3]
fig, ax = plt.subplots(figsize=(8, 8))

for a in a_values:
    plot_conchoid_of_de_sluze(ax, a)

ax.set_title('Conchoid of deSluze for different values of a')
ax.set_xlabel('x')
ax.set_ylabel('y')
ax.axhline(0, color = 'black', linewidth = 1)
ax.axvline(0, color = 'black', linewidth = 1)
ax.grid(color='gray', linestyle='--', linewidth=0.5)
ax.legend()
ax.set_xlim(-3.5, 5.0)
ax.set_ylim(-5.0, 5.0)
plt.savefig('conchoid_of_de_sluze.png')
