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
d = 0                        # no disturbance: isolate reference feedforward
kp = [0.1, 1, 10, 100]       # control gain (multiple values)
kff = .9 / ksys              # feedforward control gain

# Define the ODE for the cruise control system
def cruise_ctrl_ode(t, y, K, tau, ref_speed, d):
    speed = y[0]
    feedback_ctrl = K * (ref_speed - speed)
    feedforward_ctrl = kff * ref_speed
    acceleration = (-speed + ksys * (feedforward_ctrl + feedback_ctrl + d)) / tau
    dydt = [acceleration]
    return dydt

# Initial conditions: 50 mph.  Time span for simulation
initial_speed = 50;          # initial speed (mph)
ref_speed = 60         # desired speed (mph)
init_cond = [initial_speed]; t_span = (0, 6)

# Create a figure with two subplots
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(8, 6.4), sharex=True)
ax1.set_title('Cruise control with reference feedforward')
colors = ['#752d00', '#a43e00', '#d35000', '#ff6100']
feedforward_input = kff * ref_speed

# Solve the ODE and plot the results for each value of K
for i, K in enumerate(kp):
    solution = solve_ivp(cruise_ctrl_ode, t_span, init_cond, args=(K, tau, ref_speed, d), t_eval=np.arange(0, 6, 0.01), method='LSODA')
    time = solution.t
    speed = solution.y[0]
    feedback_ctrl = [K * (ref_speed - speed[j]) for j in range(len(time))]
    total_ctrl = [u + feedforward_input for u in feedback_ctrl]
    ax1.plot(time, speed, label=f'$k_{{\\mathrm{{p}}}} = {K}$', color=colors[i])
    ax1.set_ylabel('speed $v(t)$')
    ax1.set_xlim(0, 6); ax1.set_ylim(35, 65); ax1.grid(True); ax1.legend()
    ax2.plot(time, feedback_ctrl, label=f'$k_{{\\mathrm{{p}}}} = {K}$',
             color=colors[i])
    ax2.plot(time, total_ctrl, color=colors[i], linestyle='--', alpha=.8,
             label='_nolegend_')
    ax2.set_xlabel('time $t$')
    ax2.set_ylabel(r'control input $u(t)$')
    ax2.set_xlim(0, 6); ax2.set_ylim(0, 1100); ax2.grid(True)

ax2.axhline(feedforward_input, color='black', linestyle=':',
            label=r'$u_{\mathrm{ff}}=k_{\mathrm{ff}}r$')
ax2.legend()

plt.savefig('ch07-cruise-control-proportional-feedforward.pdf', bbox_inches='tight')
