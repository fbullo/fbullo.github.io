# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np; import matplotlib.pyplot as plt; import control as ctrl
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"], "font.size": 16 })

# Define a range of time constants and time range for the simulation
time_constants = [10, 8, 5, 3, 2, 1, 0.5] 
t = np.linspace(0, 25, 1000)  

# Define your preferred color vector
colors = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']

# Initialize the figure for impulse, step, and ramp responses
fig, axs = plt.subplots(3, 1, figsize=(10, 10))  

# Loop through each time constant and plot the impulse, step, and ramp responses
for idx, tau in enumerate(time_constants):
    # Define the transfer function of the first-order system
    num = [1]
    den = [tau, 1]
    system = ctrl.TransferFunction(num, den)

    # Compute and plot the impulse response
    t_impulse, y_impulse = ctrl.impulse_response(system, T=t)
    axs[0].plot(t_impulse, y_impulse, label=f'$\\tau = {tau}$', color=colors[idx])

    # Compute and plot the step response
    t_step, y_step = ctrl.step_response(system, T=t)
    axs[1].plot(t_step, y_step, label=f'$\\tau = {tau}$', color=colors[idx])

    # Compute and plot the ramp response
    ramp_input = t
    t_ramp, y_ramp = ctrl.forced_response(system, T=t, U=ramp_input)
    axs[2].plot(t_ramp, y_ramp, label=f'$\\tau = {tau}$', color=colors[idx])

# Add labels, legends, grid, and set xlim for all subplots
for ax in axs:
    ax.legend(fontsize=14); ax.grid(True); ax.set_xlim(0, 25); ax.set_ylabel('Response')
axs[0].set_xlim(0, 5);  axs[0].set_xticklabels([]); axs[1].set_xticklabels([]); 

# Add arrow with text
arrow_color = '#0055A4'
text_color = arrow_color  

axs[0].text(0.5, 0.5, "increasing $\\tau$", ha="center", va="center", rotation=45, size=15, color=text_color, bbox=dict(boxstyle="larrow,pad=0.3", fc="none", ec=arrow_color, lw=2, alpha=0.5))
axs[1].text(2.5, 0.75, "increasing $\\tau$", ha="center", va="center", rotation=-45, size=15, color=text_color, bbox=dict(boxstyle="rarrow,pad=0.3", fc="none", ec=arrow_color, lw=2, alpha=0.5))
axs[2].text(15, 12, "increasing $\\tau$", ha="center", va="center", rotation=-45, size=15, color=text_color, bbox=dict(boxstyle="rarrow,pad=0.3", fc="none", ec=arrow_color, lw=2, alpha=0.5))

# Plot unit step and unit ramp in gray
axs[1].plot(t, np.ones_like(t), label='Unit Step', color='#666666', linestyle='--')
axs[2].plot(t, t, label='Unit Ramp', color='#666666', linestyle='--')

axs[0].set_title(r'Impulse, step, and ramp response of 1st order system');
# axs[1].set_title(r'Step response of 1st order system, for varying time constant $\tau$');
# axs[2].set_title(r'Ramp response of 1st order system, for varying time constant $\tau$'); axs[2].set_xlabel('time $t$')

# Adjust layout and save the plot to a PDF file
plt.tight_layout(); plt.savefig('ch05-firstorder-responses.pdf', bbox_inches='tight')
