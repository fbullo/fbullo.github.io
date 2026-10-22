# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np; import matplotlib.pyplot as plt
plt.rcParams.update({"text.usetex": True, "font.family": "serif",
                     "font.serif": ["Computer Modern Roman"], "font.size": 14 })

# Define six dynamical systems as functions:
# 1. Exponentially stable linear system
def stable_system(z, t):
    x, y = z
    dxdt = -x
    dydt = -y
    return [dxdt, dydt]

# 2. Saddle point system (with unstable and stable manifolds)
def saddle_system(z, t):
    x, y = z
    dxdt = x
    dydt = -y
    return [dxdt, dydt]

# 3. Unstable system (linear system with unstable equilibrium)
def unstable_system(z, t):
    x, y = z
    dxdt = x
    dydt = y
    return [dxdt, dydt]

# 4. Periodic orbit system (nonlinear oscillator)
def periodic_system(z, t):
    x, y = z
    dxdt = y
    dydt = -x
    return [dxdt, dydt]

# 5. Multistable system (two stable points, one unstable)
def multistable_system(z, t):
    x, y = z
    dxdt = x * (1 - x**2)
    dydt = -y 
    return [dxdt, dydt]

# 6. Infinite number of equilibrium points: the origin and the unit circle,
#    as in the caption of Figure 1.9 (stable origin, repelling unit circle)
def infinite_equilibrium_system(z, t):
    x, y = z
    r2 = x**2 + y**2
    dxdt = -x * (1 - r2)
    dydt = -y * (1 - r2)
    return [dxdt, dydt]


# Generate phase portraits and save them as PDF files
def plot_phase_portrait_with_linewidth(system, title, filename, xlim=(-2, 2), ylim=(-2, 2), num_points=20, special_points=None, circle_radius=None):
    u, v = np.meshgrid(np.linspace(xlim[0], xlim[1], num_points), np.linspace(ylim[0], ylim[1], num_points))
    du, dv = np.zeros(u.shape), np.zeros(v.shape)
    t = 0  # Time is irrelevant for the phase portrait, only system state matters

    # Compute derivatives at each point in the grid
    for i in range(num_points):
        for j in range(num_points):
            du[i, j], dv[i, j] = system([u[i, j], v[i, j]], t)

    # Compute the magnitude of the velocity field for linewidth
    magnitude = np.sqrt(du**2 + dv**2)

    # Plot the stream plot (phase portrait) with variable linewidth
    fig, ax = plt.subplots(figsize=(6, 6))
    ax.streamplot(u, v, du, dv, density=0.95, color='#0085ff', linewidth=2*magnitude/magnitude.max(), arrowsize=1.5)
    ax.set_xlim(xlim)
    ax.set_ylim(ylim)
    ax.set_title(title)
    ax.grid(True)

    # Add the scatter point at the origin (0, 0)
    ax.scatter(0, 0, color='black', s=50, zorder=5)

    # Add special points if provided (like in multistable system)
    if special_points:
        for point in special_points:
            ax.scatter(point[0], point[1], color='black', s=50, zorder=5)

    # Draw a circle if radius is provided (like in infinite equilibrium system)
    if circle_radius:
        circle = plt.Circle((0, 0), circle_radius, color='#d35000', fill=False, linestyle='-', linewidth=2, zorder=5)  # gnbrown4: revised 2026-09-24
        ax.add_artist(circle)

    # Remove x and y labels as per user's request
    ax.set_xticklabels([])
    ax.set_yticklabels([])

    # Save the plot as a PDF file
    plt.savefig(f"{filename}.pdf", bbox_inches='tight')
    plt.close()

# List of systems, titles, file names, and special attributes
systems = [
    (stable_system, 'Stable equilibrium', 'ch01-phase-portraits-1', None, None),
    (saddle_system, 'Saddle point', 'ch01-phase-portraits-2', None, None),
    (unstable_system, 'Unstable equilibrium', 'ch01-phase-portraits-3', None, None),
    (periodic_system, 'Periodic orbit', 'ch01-phase-portraits-4', None, None),
    (multistable_system, 'Multiple stable equilibria', 'ch01-phase-portraits-5', [(-1, 0), (1, 0)], None),
    (infinite_equilibrium_system, 'Infinite equilibrium points', 'ch01-phase-portraits-6', None, 1)
]

# Generate and save plots for each system with variable linewidth and origin marker
for system, title, filename, special_points, circle_radius in systems:
    plot_phase_portrait_with_linewidth(system, title, filename, special_points=special_points, circle_radius=circle_radius)

