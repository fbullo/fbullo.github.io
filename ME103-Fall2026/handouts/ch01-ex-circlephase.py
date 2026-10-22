# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np; import matplotlib.pyplot as plt
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Define the system of differential equations
def system(Y, omega=0, lambd=1):
    x, y = Y
    r_squared = x**2 + y**2
    dx = omega * y + lambd * (1 - r_squared) * x
    dy = -omega * x + lambd * (1 - r_squared) * y
    return dx, dy

# Create a grid of points to compute the vector field
x_vals = np.linspace(-2, 2, 20)
y_vals = np.linspace(-2, 2, 20)
X, Y = np.meshgrid(x_vals, y_vals)

omega_values = [-1, 0, 1]  # omega<0, omega=0, omega>0
lambd = 1

for omega in omega_values:
    U, V = np.zeros(X.shape), np.zeros(Y.shape)
    NI, NJ = X.shape
    for i in range(NI):
        for j in range(NJ):
            x = X[i, j]
            y = Y[i, j]
            dx, dy = system([x, y], omega, lambd)
            U[i, j], V[i, j] = dx, dy

    # Compute the magnitude for linewidth
    magnitude = np.sqrt(U**2 + V**2) / 2

    plt.figure(figsize=(7, 7))
    # Increase the density for omega != 0 to add more arrows
    density_value = 0.5
    
    # Define a grid of start points to distribute arrows more evenly along the streamlines
    # start_points = np.array([[x, y] for x in np.linspace(-2, 2, 8) for y in np.linspace(-2, 2, 8)])

    plt.streamplot(X, Y, U, V, density=density_value, color='#0085ff', linewidth=magnitude, 
                   arrowsize=2, maxlength=1.5, broken_streamlines=False)
    plt.axhline(0, color='black', linewidth=2)  # x-axis
    plt.axvline(0, color='black', linewidth=2)  # y-axis
    plt.xlabel('$x$',fontsize=20);     plt.ylabel('$y$',fontsize=20);     plt.tight_layout()

    # Save the figure to a PDF file
    filename = f"ch01-ex-circlephase-{'neg' if omega < 0 else ''}{abs(omega)}.pdf"
    plt.savefig(filename, bbox_inches='tight')
    plt.close()  # Close the current figure to avoid displaying it

