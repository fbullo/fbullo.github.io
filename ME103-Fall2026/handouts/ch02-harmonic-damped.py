# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

# Python libraries
import numpy as np; import matplotlib.pyplot as plt; from scipy.integrate import solve_ivp
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Constants
m = 1.0   # Mass
b = 0.5   # Damping coefficient
k = 2.0   # Stiffness

# Differential equations for damped harmonic oscillator
def damped_oscillator(t, y, b, k, m):
    x, v = y
    dxdt = v
    dvdt = -(b/m) * v - (k/m) * x
    return [dxdt, dvdt]

# Time vector
t = np.linspace(0, 14, 1000)

# Five different initial conditions [x0, v0]
initial_conditions = [ [2, 0],  [1, 0],  [.5, 0],  [.1, 0], [-1, 0]]
colors = ['#471b00','#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a']

# Plotting solutions as a function of time
plt.figure(figsize=(6,3)); plt.xlim(0, 14);
for idx, init_cond in enumerate(initial_conditions):
    sol = solve_ivp(damped_oscillator, (t[0], t[-1]), init_cond, t_eval=t, args=(b, k, m), rtol=1e-8, atol=1e-10)
    plt.plot(t, sol.y[0], label=f'$x_0={init_cond[0]}$, $v_0={init_cond[1]}$', color=colors[idx])

plt.ylabel('displacement $x(t)$',fontsize=12); plt.legend(); plt.xlabel('time $t$',fontsize=12); plt.grid(True);
plt.savefig('ch02-harmonic-damped.pdf', bbox_inches='tight')

# Phase portrait
X, Y = np.meshgrid(np.linspace(-2, 2, 20), np.linspace(-3, 3, 20))
U = Y; V = -(b/m) * Y - (k/m) * X; magnitude = np.sqrt(U**2 + V**2)

plt.figure(figsize=(12,6)); plt.grid(True)
plt.xlim(-2, 2); plt.ylim(-3, 3); plt.scatter(0, 0, color='black', s=50, zorder=5) 
plt.streamplot(X, Y, U, V, density=0.75, color='#0085ff', arrowsize=1.5, linewidth=magnitude)
plt.xlabel('displacement $x$',fontsize=24); plt.ylabel('velocity $v$',fontsize=24)
plt.tick_params(axis='both', which='major', labelsize=24)
plt.savefig('ch02-harmonic-damped-phase.pdf', bbox_inches='tight')

