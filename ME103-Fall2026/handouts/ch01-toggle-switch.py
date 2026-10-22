# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

# Python libraries
import numpy as np; import matplotlib.pyplot as plt; from scipy.integrate import solve_ivp
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"], "font.size": 14 })

# Toggle switch dynamics
def toggle_switch(t, Y, alpha1, alpha2, n1, n2, beta):
    u, v = Y
    dudt = alpha1 / (1 + v**n1) - beta * u
    dvdt = alpha2 / (1 + u**n2) - beta * v
    return [dudt, dvdt]
# Parameters
alpha1, alpha2 = 1.0, 1.0  # production rates
n1 = 3.53; n2 = 3.55       # Hill coefficients
beta = 0.5                 # degradation rate
params = (alpha1, alpha2, n1, n2, beta)
# Initial conditions: [u0, v0] and Time array
initial_conditions = [ [0.35, 1.1],  [.9, 0.5],  [1, 2], [2.5, 1] ]
t = np.linspace(0, 10, 1000); plt.figure(figsize=(10,4.6))

# Solve and plot for each initial condition
colors = ['#0055A4', '#006CD4', '#a43e00', 'black']
for idx, ic in enumerate(initial_conditions):
    Y = solve_ivp(toggle_switch, (0, 10), ic, t_eval=t, args=params).y
    u, v = Y; plt.plot(t, u, color=colors[idx], label=f'$u_0={ic[0]}, v_0={ic[1]}$')
    plt.plot(t, v, '--', color=colors[idx], label=f'$u_0={ic[0]}$, $v_0={ic[1]}$')

# Annotate the plot and save to PDF (plt.xlabel('Time, t'))
# plt.title('Toggle Switch', fontsize=16)
plt.ylabel('$u (t)$ (solid) $v(t)$ (dashed)', fontsize=16);plt.legend(fontsize=10);
plt.xlabel('time $t$', fontsize=16);
plt.xlim(0, 10); plt.ylim(0, 2.25); plt.grid(True)
plt.savefig("ch01-toggle-switch.pdf", bbox_inches='tight')

# Phase portrait
u, v = np.meshgrid(np.linspace(0, 4, 20), np.linspace(0, 3, 20))
du, dv = toggle_switch(0, [u, v], alpha1, alpha2, n1, n2, beta)
magnitude = np.sqrt(du**2 + dv**2); plt.figure(figsize=(10,4.6));
plt.streamplot(u, v, du, dv, density=.75, color='#0085ff', linewidth=magnitude, broken_streamlines=False, arrowsize=1.5)
for idx, ic in enumerate(initial_conditions):
    Y = solve_ivp(toggle_switch, (0, 10), ic, t_eval=t, args=params).y
    plt.plot(Y[0], Y[1], color=colors[idx], label=f'u0={ic[0]}, v0={ic[1]}')
eq_points = np.array([[1., 1.], [0.17, 2.], [2., 0.17]]) 
plt.scatter(eq_points[:, 0], eq_points[:, 1], color='black', s=50, zorder=5)
plt.xlabel('$u(t)$', fontsize=16); plt.ylabel('$v(t)$', fontsize=16); plt.xlim(0, 4); plt.ylim(0, 3)
plt.savefig("ch01-toggle-switch-phase.pdf", bbox_inches='tight')
