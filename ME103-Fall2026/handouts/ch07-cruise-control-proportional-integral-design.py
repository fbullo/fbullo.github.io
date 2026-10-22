# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np; import matplotlib.pyplot as plt; from scipy.integrate import solve_ivp; plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"]})

# Constants
ksys = 3                     # system gain, we let kdist=ksys
tau = 5                      # Plant time constant (slow system)
d = -10                      # Disturbance
# desired natural frequency and damping ratio:
omegan_desired = 1
zeta_desired = .6
# note: zeta_desired > 1/(2 tau omegan_desired)  = 1/(2 x 5) = 1/10.
ki = tau * omegan_desired**2 / ksys
kp = (2 * tau * omegan_desired * zeta_desired - 1) / ksys

# Define the ODE for the cruise control system
def cruise_control_ode(t, y, kp, ki, tau, reference_speed, d):
    speed, integral = y
    error = reference_speed - speed
    control_input = kp * error + ki * integral
    acceleration = (-speed + ksys * (control_input + d)) / tau
    dydt = [acceleration, error]
    return dydt

# Initial conditions and time span
reference_speed = 60               # Desired speed (mph)
initial_speeds = [40, 50, 55, 65]  # Various initial speeds
t_span = (0, 10) 

# Create a figure with two subplots
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8), sharex=True)
ax1.set_title('Car dynamics under proportional-integral control with tuned gains'); colors = ['#752d00', '#a43e00', '#d35000', '#ff6100']

# Solve the ODE and plot the results for each initial speed
for i, initial_speed in enumerate(initial_speeds):
    initial_conditions = [initial_speed, 0]  # v(0) and integral state z(0)
    solution = solve_ivp(cruise_control_ode, t_span, initial_conditions,
                         args=(kp, ki, tau, reference_speed, d), 
                         t_eval=np.arange(0, 10, 0.01), method='LSODA')
    time = solution.t; speed = solution.y[0]
    control_input = [kp * (reference_speed - speed[j]) + ki * solution.y[1][j] for j in range(len(time))]

    # Plot the speed in the first subplot and control signal in the second
    ax1.plot(time, speed, label=f'Initial Speed = {initial_speed} mph', color=colors[i]); ax1.set_ylabel('speed (mph)'); ax1.set_xlim(0, 10); ax1.set_ylim(30, 70); ax1.grid(True); ax1.legend()
    ax2.plot(time, control_input, label=f'Initial Speed = {initial_speed} mph', color=colors[i]); ax2.set_xlabel('time $t$'); ax2.set_ylabel('signed control input $u(t)$'); ax2.set_xlim(0, 10); ax2.set_ylim(-15, 50); ax2.grid(True)

# Save the plot to a PDF file
ax2.legend()
plt.savefig('ch07-cruise-control-proportional-integral-design.pdf', bbox_inches='tight')
