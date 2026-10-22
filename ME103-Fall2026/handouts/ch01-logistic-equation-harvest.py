# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

# ╔════════════════════════════════════════════════════════════╗
# ║  Logistic growth with constant harvest + equilibrium lines  ║
# ╚════════════════════════════════════════════════════════════╝

import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp

# ── Optional LaTeX text rendering ─────────────────────────────
plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"]
})

# ── Model definition ──────────────────────────────────────────
def logistic_equation(t, x, r, kappa, h):
    """ dx/dt = r·x·(1 – x/kappa) – h """
    return r * x * (1 - x / kappa) - h

# ── Parameters ────────────────────────────────────────────────
r = 0.1    # intrinsic growth rate
kappa = 100    # carrying capacity
h = 2      # constant harvest rate

# ── Equilibrium points (solve quadratic) ─────────────────────
#     (kappa ± sqrt(kappa^2 - 4·kappa/r·h)) / 2
disc = kappa**2 - 4 * (kappa / r) * h
equilibria = []
if disc >= 0:               # real equilibria exist
    equilibria = [ (kappa + np.sqrt(disc)) / 2,
                   (kappa - np.sqrt(disc)) / 2 ]

# ── Simulation settings ───────────────────────────────────────
# initial_conditions = [1, 5, 10, 25, 50, 75, 125, 150, 175]
initial_conditions = [
     1,    3,    5,    7.5,  10,   17.5,   25,   37.5,   50,
    62.5, 75,  100,  125,  137.5, 150,  162.5, 175,  190 ]
T  = 125               # final time
dt = 0.1
times = np.arange(0, T + dt, dt)   # evaluation grid

# One distinct palette color for each initial condition.
colors = [
    '#002447', '#003c76', '#0055A4', '#006CD4', '#0085ff', '#239cff', '#58b1ff',
    '#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56',
    '#1a1a1a', '#333333', '#4d4d4d', '#666666',
]

# ── Numerical integration & plotting ──────────────────────────
plt.figure(figsize=(10, 5))

# plot trajectories
for i, x0 in enumerate(initial_conditions):
    sol = solve_ivp(logistic_equation,
                    (0, T),
                    [x0],
                    t_eval=times,
                    args=(r, kappa, h))

    plt.plot(sol.t, sol.y[0],
             label=fr'initial $x_0={x0}$',
             color=colors[i])

# horizontal lines at equilibrium values
for eq in equilibria:
    plt.axhline(y=eq, linestyle='--', linewidth=1,
                label=fr'equilibrium $x^\ast={eq:.2f}$')

# plt.title(r'Numerical solutions of $\dot x = r\,x(1-\tfrac{x}{kappa}) - h$')
plt.xlabel(r'time $t$')
plt.ylabel(r'population size $x(t)$')

plt.xlim(0, 120)
plt.ylim(0, 180)
plt.grid(True)
plt.legend(loc='upper right', fontsize='small')
plt.tight_layout()

# ── Save to PDF (optional) ────────────────────────────────────
plt.savefig("ch01-logistic-equation-harvest.pdf", bbox_inches='tight')
# plt.show()

