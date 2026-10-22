# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
plt.rcParams.update({"text.usetex": True, "font.family": "serif",
                     "font.serif": ["Computer Modern Roman"], "font.size": 14 })

def lotka_volterra_system(X, alpha):
    x1, x2 = X
    dx1 = x1 - x1**2 + alpha * x1 * x2
    dx2 = x2 - 2*x2**2 + alpha * x1 * x2
    return [dx1, dx2]

# Define the range of values for x1 and x2
x = np.linspace(0, 2, 20)  # Adjust as needed for resolution
y = np.linspace(0, 2, 20)  # Adjust as needed for resolution
X, Y = np.meshgrid(x, y)

# List of interaction parameters
alphas = [0, -0.5, 0.5, 2, -0.9, -1.1, -2]  # <-- added -0.9 and -1.1

for alpha in alphas:
    # Evaluate system on grid
    U, V = np.zeros(X.shape), np.zeros(Y.shape)
    NI, NJ = X.shape

    for i in range(NI):
        for j in range(NJ):
            x = X[i, j]
            y = Y[i, j]
            dx1, dx2 = lotka_volterra_system([x, y], alpha)
            U[i,j] = dx1
            V[i,j] = dx2

    # Calculate the magnitude of the vector field
    magnitude = np.sqrt(U**2 + V**2)

    plt.figure(figsize=(7, 7))
    plt.xlim(0, 2); plt.ylim(0, 2)
    plt.streamplot(X, Y, U, V, color='#0085ff', arrowsize=2,
                   density=0.65, linewidth=magnitude,
                   broken_streamlines=False)
    
    # Adding vertical and horizontal dashed lines
    line1 = plt.axvline(x=1, color='k', linestyle='--', linewidth=0.5)
    line2 = plt.axhline(y=1/2, color='k', linestyle='--', linewidth=0.5)
    line1.set_dashes([10, 5])
    line2.set_dashes([10, 5])
    
    plt.xlabel('$x_1$', fontsize=24)
    plt.ylabel('$x_2$', fontsize=24)
    plt.title(rf"$\alpha = {alpha}$")  # <-- added informative title
    
    plt.savefig(f'ch01-ex-lotkavolterra-param-{str(alpha).replace('-','neg').replace('.','p')}.pdf', bbox_inches='tight')
    plt.close()
