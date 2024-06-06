from sympy import symbols, groebner
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits import mplot3d

u, v, x, y, z = symbols('u v x y z')

f1 = u * v - x
f2 = v - y
f3 = u**2 - z

G = groebner([f1, f2, f3], u, v, x, y, z, order='lex')

print("Baza Groebnera dla równań parametrycznych Parasolki Whitney'a:")
counter = 1
for g in G:
    print(f"{counter}. {g}")
    counter += 1

u = np.linspace(-2, 2, 1000)
v = np.linspace(-2, 2, 1000)

U, V = np.meshgrid(u, v)

X = U * V
Y = U
Z = V ** 2

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.plot_surface(X, Y, Z, cmap='viridis')

ax.set_xlabel('X')
ax.set_ylabel('Y')
ax.set_zlabel('Z')
ax.set_title("Whitney's Umbrella")

ax.view_init(elev=30, azim=30)
plt.grid(True)
plt.savefig('whitney_umbrella.png')

