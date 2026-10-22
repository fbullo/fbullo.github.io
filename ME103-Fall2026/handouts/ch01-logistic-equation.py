# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

# Python libraries
import numpy as np; import matplotlib.pyplot as plt; from scipy.integrate import solve_ivp
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Logistic equation: solve_ivp expects f(t, y, *args)
def logistic_equation(t, x, r, kappa):
    return r * (1 - x / kappa) * x

# Parameters 
r = 0.1   # growth rate
kappa = 100   # carrying capacity

# Initial conditions and time range of integration
initial_conditions = [1, 5, 10, 25, 50, 75, 125, 150, 175]
T = 120.0         # maximum time
dt = 0.1          # time step 
times = np.linspace(0.0, T, int(T/dt) + 1)
colors = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']

# Numerically solve the ODE and plot solution
plt.figure(figsize=(6, 2.7))
for i, x0 in enumerate(initial_conditions):
    sol = solve_ivp( logistic_equation, t_span=(0, T),
        y0=[x0], args=(r, kappa), t_eval=times,
        rtol=1e-8, atol=1e-10, method="RK45")  
    plt.plot(sol.t, sol.y[0], label=f'$x(0)= {x0}$', color=colors[i % len(colors)])

# Labels, legend, axes
plt.xlabel(r"time $t$"); plt.ylabel(r"population size $x(t)$")
plt.legend(loc='lower right', fontsize=6); plt.grid(True); 
plt.xlim(0, 120); plt.ylim(0, 180)

# Save to PDF
plt.savefig("ch01-logistic-equation.pdf", bbox_inches="tight")
