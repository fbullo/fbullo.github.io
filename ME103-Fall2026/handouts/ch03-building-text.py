# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np; import matplotlib.pyplot as plt; import matplotlib
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

def heat_flow_dynamics(T1, T2, T3, c1, c2, c3, r12, r23, k, Tac, Text):
    u = 0 # no control = no air conditioning
    T1_dot = (T2 - T1) / (r12 * c1) + (Text - T1) / (r1ext * c1)
    T2_dot = (T1 - T2) / (r12 * c2) + (T3 - T2) / (r23 * c2)
    T3_dot = (T2 - T3) / (r23 * c3) + k * u * (Tac - T3) / c3
    return T1_dot, T2_dot, T3_dot, u

# Parameters
c1, c2, c3, r12, r23, r1ext, Text, Tac, k = 1000, 1000, 1000, 0.2, 0.9, .5, 30, 20, 3.0
t_end = 5400; dt = t_end / 24000; times = np.arange(0, t_end + dt, dt)

# Initialize values
T1, T2, T3 = 15.0, 18.0, 27.0
T1_vals, T2_vals, T3_vals, u_vals = [T1], [T2], [T3], [0]
for t in times[1:]:
    T1_dot, T2_dot, T3_dot, u = heat_flow_dynamics(T1, T2, T3, c1, c2, c3, r12, r23, k, Tac, Text)
    T1 += dt * T1_dot; T2 += dt * T2_dot; T3 += dt * T3_dot
    T1_vals.append(T1); T2_vals.append(T2); T3_vals.append(T3); u_vals.append(u)

# Create the figure and the gridspec
fig = plt.figure(figsize=(9, 6)); gs = matplotlib.gridspec.GridSpec(2, 1, height_ratios=[3, 1])
blues = ['#002447', '#003c76', '#0055A4', '#006CD4', '#0085ff', '#239cff', '#58b1ff']
orngs = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']

# First subplot (Temperatures)
ax1 = plt.subplot(gs[0]); ax1.set_ylim(15, 28)
ax1.plot(times, T1_vals, label='Temperature of Room 1 (C)', color=blues[1])
ax1.plot(times, T2_vals, label='Temperature of Room 2 (C)', color=blues[3])
ax1.plot(times, T3_vals, label='Temperature of Room 3 (C)', color=blues[5])
ax1.grid(True); ax1.legend(); ax1.set_ylabel('Temperature (C)')
ax1.set_title('Temperatures of the rooms and AC control signal over 1.5 hours')
ax1.set_yticks(np.arange(15, max(max(T1_vals), max(T2_vals), max(T3_vals)) + 1, 1))
ax1.set_xlim(0, t_end); ax1.set_xticks([0, 900, 1800, 2700, 3600, 4500, 5400]) 
ax1.set_xticklabels(['0', '15 min', '30 min', '45 min', '60 min', '75 min', '90 min'])
ax1.text(900, 25.5, r'$T_1(t)$', fontsize=14, ha='center', va='center', bbox=dict(facecolor='white', edgecolor='none', pad=5.0))
ax1.text(1800, 27, r'$T_2(t)$', fontsize=14, ha='center', va='center', bbox=dict(facecolor='white', edgecolor='none', pad=5.0))
ax1.text(2700, 26.75, r'$T_3(t)$', fontsize=14, ha='center', va='center', bbox=dict(facecolor='white', edgecolor='none', pad=5.0))

# Second subplot (Control signal)
ax2 = plt.subplot(gs[1]); ax2.set_xlim(0, t_end); ax2.grid(True)
ax2.plot(times, u_vals, label='Control signal (u)', color='black')
ax2.set_xlabel('time $t$'); ax2.set_ylabel('control signal')
ax2.set_xticks([0, 900, 1800, 2700, 3600, 4500, 5400]) 
ax2.set_xticklabels(['0', '15 min', '30 min', '45 min', '60 min', '75 min', '90 min'])
ax2.set_yticks([0, 1]); ax2.set_yticklabels(['OFF', 'ON']); ax2.set_ylim(-0.1, 1.1)

# Save the figure
plt.tight_layout(); plt.savefig("ch03-building-text.pdf", bbox_inches='tight')
