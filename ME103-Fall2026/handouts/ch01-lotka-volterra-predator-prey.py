# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

# Python libraries
import numpy as np; import matplotlib.pyplot as plt;
from scipy.integrate import solve_ivp
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"], "font.size": 16})

# Define the Lotka-Volterra equations
alpha = 0.1; beta = 0.02; gamma = 0.01; delta = 0.1
def lotka_volterra(t, z, alpha, beta, gamma, delta):
    x, y = z
    dxdt = alpha * x - beta * x * y
    dydt = -delta * y + gamma * x * y
    return [dxdt, dydt]

# Solve the equations:
initial_condition = (20, 5)
T = 200; dt = 0.1
times = np.linspace(0.0, T, int(T/dt) + 1)
sol = solve_ivp(lotka_volterra,
    t_span=(times[0], times[-1]),
    y0=initial_condition,
    args=(alpha, beta, gamma, delta),
    t_eval=times,
    rtol=1e-8, atol=1e-10, method="RK45")

# Plot the solution as a function of time
plt.figure(figsize=(10, 5)); 
plt.plot(sol.t, sol.y[0], label='prey')
plt.plot(sol.t, sol.y[1], label='predator')
plt.xlabel(r"time $t$"); plt.ylabel("population size"); plt.legend()
plt.grid(True); plt.xlim(0, 200); plt.ylim(0, 22.25)
plt.savefig("ch01-lotka-volterra-predator-prey.pdf", bbox_inches='tight')

# Phase portrait
u, v = np.meshgrid(np.linspace(0, 25, 20), np.linspace(0, 15, 20))
du, dv = lotka_volterra(0, [u, v], alpha, beta, gamma, delta)
magnitude = 2*np.sqrt(du**2 + dv**2)

plt.figure(figsize=(10, 5)); plt.xlim(-0.3, 25); plt.ylim(-0.3, 15)
plt.streamplot(u, v, du, dv, density=.95, color='#0085ff', linewidth=magnitude, arrowsize=2)

# Add the specific solution trajectory
plt.plot(sol.y[0], sol.y[1], color='#752d00', label='Trajectory from (20,5)')
plt.scatter(10, 5, color='black', s=50, zorder=5);
plt.scatter(0, 0, color='black', s=50, zorder=5);
plt.grid(True)
plt.xlabel('prey population $x_1(t)$')
plt.ylabel('predator population $x_2(t)$'); 
plt.savefig("ch01-lotka-volterra-predator-prey-phase.pdf", bbox_inches='tight')
