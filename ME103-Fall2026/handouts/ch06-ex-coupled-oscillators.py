# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"],
 "font.size": 18,            # base font size
    "axes.titlesize": 18,       # title
    "axes.labelsize": 18,       # x/y labels
    "xtick.labelsize": 16,      # x ticks
    "ytick.labelsize": 16,      # y ticks
    "legend.fontsize": 16,      # legend
                     })

# System parameters
m1, m2 = 1.0, 1.0  # Identical masses
k1, k2 = 5.0, 5.0  # Identical spring constants
kcoupling = 0.25  # Weak coupling spring

# Differential equations
def equations(t, y):
    x1, x2, v1, v2 = y
    dx1dt = v1
    dx2dt = v2
    dv1dt = (-k1 * x1 + kcoupling * (x2 - x1)) / m1
    dv2dt = (-k2 * x2 + kcoupling * (x1 - x2)) / m2
    return [dx1dt, dx2dt, dv1dt, dv2dt]

# Initial conditions: x1(0)=1, x2(0)=0, v1(0)=0, v2(0)=0
initial_conditions = [1.0, 0.0, 0.0, 0.0]

# Time points
t = np.linspace(0, 200, 50000)

# Solve ODE
solution = solve_ivp(equations, (t[0], t[-1]), initial_conditions, t_eval=t, rtol=1e-8, atol=1e-10)

# Plot results
plt.figure(figsize=(11, 4))
plt.plot(t, solution.y[0], label=r'$x_1(t)$')
plt.plot(t, solution.y[1], label=r'$x_2(t)$')
plt.title(r'Two identical harmonic oscillators connected by a weak spring ($x_1(0)=1, x_2(0)=\dot{x}_1(0)=\dot{x}_2(0)=0$)')
plt.xlabel('time $t$'); plt.ylabel('positions $x_1(t)$ and $x_2(t)$')
plt.legend(); plt.grid(True); plt.xlim([0,200])

# Adjust layout and save the plot to a PDF file
plt.tight_layout(); plt.savefig('ch06-ex-coupled-oscillators.pdf', bbox_inches='tight')
