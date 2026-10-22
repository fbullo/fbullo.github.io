# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp
plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"]
})


# Constants
K = 4.1  # Spring constant K
M = 1.0  # Mass M

k = 0.4 # Spring constant k
m = 0.1  # Mass m

P0 = 2.0 # Amplitude of driving force
omega = 2.0 # Frequency of driving force

# Differential equations
def system(t, y):
    x1, v1, x2, v2 = y
    dv1dt = (P0 * np.sin(omega * t) - K * x1 - k * (x1-x2) ) / M
    dv2dt = - k * (x2 - x1) / m
    return [v1, dv1dt, v2, dv2dt]

# Initial conditions: [x1(0), x1_dot(0), x2(0), x2_dot(0)]
y0 = [0.0, 0.0, 0.0, -omega * P0 / k]

# Time range for the solution
t_span = [0, 20]
t_eval = np.linspace(*t_span, 50000)

# Solve the system
sol = solve_ivp(system, t_span, y0, t_eval=t_eval,rtol=1e-8, atol=1e-10)

# Plotting
plt.figure(figsize=(9, 5))

# x1 vs time
plt.subplot(2, 1, 1)
plt.plot(sol.t, sol.y[0], label='$x_1(t)$')
plt.title('displacements $x_1(t)$ and $x_2(t)$ over time')
plt.xlabel('time $t$')
plt.ylabel('$x_1(t)$')
plt.grid(True)
plt.legend()

# x2 vs time
plt.subplot(2, 1, 2)
plt.plot(sol.t, sol.y[2], label='$x_2(t)$', color='#ff6100')
plt.xlabel('time $t$')
plt.ylabel('$x_2(t)$')
plt.grid(True)
plt.legend()
plt.tight_layout()

plt.savefig(f'ch06-ex-vibration-absorber.pdf', bbox_inches='tight')
plt.close()  # Close the current figure to avoid displaying it
