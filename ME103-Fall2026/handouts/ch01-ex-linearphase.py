# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt

# Define the system of differential equations
def system(Y, a=1):
    x, y = Y
    dx = a * x
    dy = -y
    return dx, dy

# Create a grid of points to compute the vector field
x = np.linspace(-2, 2, 20)
y = np.linspace(-2, 2, 20)
X, Y = np.meshgrid(x, y)

a_values = [-2, -1, -0.5, 0, 1, 5]

for a in a_values:
    U, V = np.zeros(X.shape), np.zeros(Y.shape)
    NI, NJ = X.shape
    for i in range(NI):
        for j in range(NJ):
            x = X[i, j]
            y = Y[i, j]
            dx, dy = system([x, y], a)
            U[i,j], V[i,j] = dx, dy

    # Compute the magnitude for linewidth
    magnitude = np.sqrt(U**2 + V**2)

    plt.figure(figsize=(7, 7))
    plt.streamplot(X, Y, U, V, density=.65, color='#0085ff', linewidth=magnitude, arrowsize=2,broken_streamlines=False)
    # plt.grid(True);
    plt.tight_layout()
    plt.axhline(0, color='black', linewidth=2)  # x-axis
    plt.axvline(0, color='black', linewidth=2)  # y-axis
    plt.savefig(f'ch01-ex-linearphase-{str(a).replace('-','neg').replace('.','p')}.pdf', bbox_inches='tight')
    plt.close()  # Close the current figure to avoid displaying it

