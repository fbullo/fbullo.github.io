# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np; import matplotlib.pyplot as plt;
from scipy.integrate import solve_ivp
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Constants
ksys = 3                     # system gain, we let kdist=ksys
tau = 5                      # Plant time constant (slow system)
d = -10                      # Disturbance
kp = 1                       # Proportional control gain 
ki = [0.1, 1, 10, 100]       # Integral control gain (multiple values)

# Define the ODE for the cruise control system
def cruise_control_ode(t, y, kp, KI, tau, reference_speed, d):
    speed, integral = y
    error = reference_speed - speed
    control_input = kp * error + KI * integral
    acceleration = (-speed + ksys*(control_input + d)) / tau
    dydt = [acceleration, error]
    return dydt

# Initial conditions and time span
reference_speed = 60         # Desired speed (mph)
initial_conditions = [50, 0] # initial speed v(0) and initial integral state z(0)
t_span = (0, 12) 

# Create a figure with two subplots
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8), sharex=True)
ax1.set_title('Car dynamics under proportional-integral control')
colors = ['#752d00', '#a43e00', '#d35000', '#ff6100']

# Solve the ODE and plot the results for each value of K
for i, Ki in enumerate(ki):
    solution = solve_ivp(cruise_control_ode, t_span, initial_conditions,
args=(kp, Ki, tau, reference_speed, d), t_eval=np.arange(0, 12, 0.01),
                         method='LSODA')
    time = solution.t;  speed = solution.y[0]
    control_input = [kp * (reference_speed - speed[j]) + Ki * solution.y[1][j] for j in range(len(time))]

    # Plot the speed in the first subplot
    ax1.plot(time, speed, label=f'$k_{{\\mathrm{{I}}}} = {Ki}$', color=colors[i])
    ax1.set_ylabel('Speed (mph)')
    ax1.set_xlim(0, 12); ax1.set_ylim(40, 70); ax1.grid(True); ax1.legend()

    # Plot the control input in the second subplot
    ax2.plot(time, control_input, label=f'$k_{{\\mathrm{{I}}}} = {Ki}$', color=colors[i])
    ax2.set_xlabel('time $t$');     ax2.set_ylabel('control input');    ax2.set_xlim(0, 12);    ax2.set_ylim(-80, 160);    ax2.grid(True)

ax2.legend()

# Save the plot to a PDF file
plt.savefig('ch07-cruise-control-proportional-integral-2.pdf', bbox_inches='tight')



