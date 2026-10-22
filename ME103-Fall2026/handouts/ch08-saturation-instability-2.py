# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Define the saturation function
def saturation(x):
    return np.clip(x, -1, 1)

# Define the vector field
def dx_dt(x, v):
    return v

def dv_dt(x, v):
    return v + saturation(-2*v-x)

# Define the ODE system
def ode_system(t, z):
    x, v = z
    return [v, v + saturation(-2*v-x)]

# Create a grid of points for the streamplot
x = np.linspace(-3, 3, 100)
v = np.linspace(-3, 3, 100)
X, V = np.meshgrid(x, v)

# Compute the vector field
U = dx_dt(X, V)
W = dv_dt(X, V)

# Time span for the simulation
t_span = (0, 10)
t_eval = np.linspace(*t_span, 500)

# Initial conditions (x, 0) with positive x only
positive_x = np.linspace(0, 3, 14)
positive_initial_conditions = [[x, x] for x in positive_x]

# Solve the ODE for each positive initial condition
positive_solutions = [solve_ivp(ode_system, t_span, ic, t_eval=t_eval) for ic in positive_initial_conditions]

# Save the plot to a PDF file
plt.figure(figsize=(8, 6))
plt.streamplot(X, V, U, W, color=np.sqrt(U**2 + W**2), cmap='RdBu_r', linewidth=1)
plt.axhline(0, color='black', linewidth=0.8, linestyle='--', alpha=0.7)
plt.axvline(0, color='black', linewidth=0.8, linestyle='--', alpha=0.7)
plt.xlim(-3, 3)
plt.ylim(-3, 3)

# Plot the trajectories from the ODE simulation
for sol in positive_solutions:
    plt.plot(sol.y[0], sol.y[1], color='#471b00', linewidth=1.5, alpha=0.8)

plt.xlabel('$x$')
plt.ylabel(r'$\dot{x}$')
plt.title('Streamplot of unstable system due to rate-limited controls')
plt.colorbar(label='Magnitude of the vector field')
plt.grid(alpha=0.3)

# Save the figure
plt.savefig("ch08-saturation-instability-2.pdf", format='pdf',bbox_inches='tight')


