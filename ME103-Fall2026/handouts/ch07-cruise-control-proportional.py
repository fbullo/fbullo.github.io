# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np; import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Constants
ksys = 3                     # system gain, we let kdist=ksys
tau = 5                      # system time constant (slow system)
d = 0                        # disturbance
kp = [0.1, 1, 10, 100]       # control gain (multiple values)

# Define the ODE for the cruise control system
def cruise_control_ode(t, y, K, tau, reference_speed, d):
    speed = y[0]
    control_input = K * (reference_speed - speed)
    acceleration = (-speed + ksys * (control_input + d)) / tau
    dydt = [acceleration]
    return dydt

# Initial conditions: 50 mph.  Time span for simulation
initial_speed = 50;          # initial speed (mph)
reference_speed = 60         # desired speed (mph)
init_cond = [initial_speed]; t_span = (0, 6)

# Create a figure with two subplots
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(8, 6.4), sharex=True)
ax1.set_title('Cruise control for car dynamics with disturbance: proportional controller')
colors = ['#752d00', '#a43e00', '#d35000', '#ff6100']

# Solve the ODE and plot the results for each value of K
for i, K in enumerate(kp):
    solution = solve_ivp(cruise_control_ode, t_span, init_cond, args=(K, tau, reference_speed, d), t_eval=np.arange(0, 6, 0.01), method='LSODA')
    time = solution.t; speed = solution.y[0]
    control_input = [K * (reference_speed - speed[j]) for j in range(len(time))]

    # Plot the speed in the first subplot and control input in the second subplot
    ax1.plot(time, speed, label=f'$k_{{\\mathrm{{p}}}} = {K}$', color=colors[i])
    ax1.set_ylabel('speed $v(t)$'); ax1.set_xlim(0, 6);  ax1.set_ylim(35, 65); ax1.grid(True); ax1.legend()
    ax2.plot(time, control_input, label=f'$k_{{\\mathrm{{P}}}} = {K}$', color=colors[i])
    ax2.set_xlabel('time $t$');  ax2.set_ylabel('control input $u(t)$'); ax2.set_xlim(0, 6); ax2.set_ylim(0, 1100); ax2.grid(True)

ax2.legend()

# Save the plot to a PDF file
plt.savefig('ch07-cruise-control-proportional.pdf', bbox_inches='tight')
