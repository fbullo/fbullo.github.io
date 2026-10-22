# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np; from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

def heat_flow_dynamics(t, state, c1, c2, c3, r12, r23):
    T1, T2, T3 = state
    T1_dot = (T2 - T1) / (r12 * c1)
    T2_dot = (T1 - T2) / (r12 * c2) + (T3 - T2) / (r23 * c2)
    T3_dot = (T2 - T3) / (r23 * c3)
    return [T1_dot, T2_dot, T3_dot]

# Parameters for the heat flow system
c1 = 1000  # Heat capacity of room 1 (J/C)
c2 = 1000  # Heat capacity of room 2 (J/C)
c3 = 1000  # Heat capacity of room 3 (J/C)
r12 = 0.2  # Thermal resistance between rooms 1 and 2 (C/W)
r23 = 0.9  # Thermal resistance between rooms 2 and 3 (C/W)

# Time array and initial conditions: [T1, T2, T3]
t = np.linspace(0, 1800, 12000)
initial_conditions = [20.0, 28.0, 35.0]
sol = solve_ivp(heat_flow_dynamics, [t[0], t[-1]], initial_conditions, t_eval=t, args=(c1, c2, c3, r12, r23), method='RK45')

# Plotting
plt.figure(figsize=(9, 5))
blues = ['#002447', '#003c76', '#0055A4', '#006CD4', '#0085ff', '#239cff', '#58b1ff']; oranges = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', 'ff9b56']
plt.plot(sol.t, sol.y[0], label='Temperature of Room 1 (Celsius)', color=blues[1])
plt.plot(sol.t, sol.y[1], label='Temperature of Room 2 (Celsius)', color=blues[3])
plt.plot(sol.t, sol.y[2], label='Temperature of Room 3 (Celsius)', color=blues[5])

# Add LaTeX labels to the plot
plt.text(300, 24.5, r'$T_1(t)$', fontsize=14, ha='center', va='center', bbox=dict(facecolor='white', edgecolor='none', pad=5.0))
plt.text(600, 26.6, r'$T_2(t)$', fontsize=14, ha='center', va='center', bbox=dict(facecolor='white', edgecolor='none', pad=5.0))
plt.text(900, 29.5, r'$T_3(t)$', fontsize=14, ha='center', va='center', bbox=dict(facecolor='white', edgecolor='none', pad=5.0))

plt.grid(True); plt.xlabel('time $t$'); plt.ylabel('temperature (Celsius)')
plt.xticks(np.arange(0, 1801, 300), ['0', '5 min', '10 min', '15 min', '20 min', '25 min', '30 min'])
plt.legend(); plt.xlim(0, 1800); plt.title('Temperatures of the rooms')
plt.tight_layout(); plt.savefig("ch03-building.pdf", bbox_inches='tight')

