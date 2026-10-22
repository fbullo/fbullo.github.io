# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np; from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt; import matplotlib.gridspec as gridspec
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

def heat_flow_dynamics(t, state, c1, c2, c3, r12, r23, k, Tac, Text):
    T1, T2, T3 = state
    u = 1.0 if T3 > 23.0 else 0.0    ## on off control
    air_conditioning = k * u * (Tac - T3)
    T1_dot = (T2 - T1) / (r12 * c1) + (Text - T1) / (r1ext * c1)
    T2_dot = (T1 - T2) / (r12 * c2) + (T3 - T2) / (r23 * c2)
    T3_dot = (T2 - T3) / (r23 * c3) + air_conditioning / c3
    return [T1_dot, T2_dot, T3_dot, u]

# Parameters for the heat flow system
c1, c2, c3, r12, r23, r1ext, k = 1000, 1000, 1000, 0.2, 0.9, .5, 3.0 
Text, Tac = 30, 20; initial_conditions = [17.0, 18.0, 27.0]
t = np.linspace(0, 1800, 18000)  

def wrap_dynamics(t, y): 
    return heat_flow_dynamics(t, y, c1, c2, c3, r12, r23, k, Tac, Text)[:3]

sol = solve_ivp(wrap_dynamics, [t[0], t[-1]], initial_conditions, t_eval=t, method='LSODA'); u_values = [heat_flow_dynamics(ti, y, c1, c2, c3, r12, r23, k, Tac, Text)[3] for ti, y in zip(sol.t, sol.y.T)]

# Create the figure and the gridspec
fig = plt.figure(figsize=(9, 6)); gs = gridspec.GridSpec(2, 1, height_ratios=[3, 1])
blues = ['#002447', '#003c76', '#0055A4', '#006CD4', '#0085ff', '#239cff', '#58b1ff']
oranges = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', 'ff9b56']

# First subplot (Temperatures)
ax1 = plt.subplot(gs[0])
ax1.plot(sol.t, sol.y[0], label='Temperature of Room 1 (C)', color=blues[1])
ax1.plot(sol.t, sol.y[1], label='Temperature of Room 2 (C)', color=blues[3])
ax1.plot(sol.t, sol.y[2], label='Temperature of Room 3 (C)', color=blues[5])
ax1.axhline(y=23, color=oranges[4], linestyle='--', linewidth=1.5,
            label='Target Temperature (23C)')
ax1.grid(True); ax1.set_ylabel('Temperature (C)'); ax1.legend()
ax1.set_title('Temperatures of the rooms and AC control signal over 30 minutes')

# Add LaTeX labels to the plot
ax1.text(150, 20.5, r'$T_1(t)$', fontsize=14, ha='center', va='center', bbox=dict(facecolor='white', edgecolor='none', pad=5.0))
ax1.text(300, 20.5, r'$T_2(t)$', fontsize=14, ha='center', va='center', bbox=dict(facecolor='white', edgecolor='none', pad=5.0))
ax1.text(800, 22.5, r'$T_3(t)$', fontsize=14, ha='center', va='center', bbox=dict(facecolor='white', edgecolor='none', pad=5.0))

ax1.set_yticks(np.arange(17, max(max(sol.y[0]), max(sol.y[1]), max(sol.y[2])) + 1, 1))
ax1.set_xlim(0, 1800); ax1.set_xticks([0, 600, 1200, 1800])
ax1.set_xticklabels(['0', '10 min', '20 min', '30 min'])

# Second subplot (Control signal)
ax2 = plt.subplot(gs[1])
ax2.plot(sol.t, u_values, label='Control signal (u)', color='black')
ax2.grid(True); ax2.set_xlabel('time $t$'); ax2.set_ylabel('control signal')
ax2.set_xticks([0, 600, 1200, 1800]); ax2.set_xlim(0, 1800)
ax2.set_xticklabels(['0', '10 min', '20 min', '30 min'])
ax2.set_yticks([0, 1]); ax2.set_yticklabels(['OFF', 'ON']); ax2.set_ylim(-0.1, 1.1)
plt.tight_layout(); plt.savefig("ch03-building-onoff.pdf", bbox_inches='tight')

