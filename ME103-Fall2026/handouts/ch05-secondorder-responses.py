# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np; import matplotlib.pyplot as plt; import control as ctrl
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"], "font.size": 16 })

# Define the parameters of the system
natural_frequency = 1.0  # Natural frequency, omega_n
damping_ratios = [0.0, 0.2, 0.4, 0.8, 1.0, 1.5, 3.0]  # Damping ratios, zeta

# Define time range for the simulation
t = np.linspace(0, 12, 1000); ramp_input = t  # Unit ramp input
colors = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']

# Initialize the figure for impulse, step, and ramp response
fig, axs = plt.subplots(3, 1, figsize=(10, 10))

# Loop through each damping ratio and plot impulse, step, and ramp responses
for idx, zeta in enumerate(damping_ratios):
    # Define the transfer function of the second-order system
    num = [natural_frequency**2]; den = [1, 2 * zeta * natural_frequency, natural_frequency**2]
    system = ctrl.TransferFunction(num, den)
    
    # Compute and plot the impulse response
    t_impulse, y_impulse = ctrl.impulse_response(system, T=t)
    line_style = '--' if idx == 4 else '-'
    axs[0].plot(t_impulse, y_impulse, line_style, label=f'$\\zeta$ = {zeta}', color=colors[idx])

    # Compute and plot the step response
    t_step, y_step = ctrl.step_response(system, T=t)
    axs[1].plot(t_step, y_step, line_style, label=f'$\\zeta$ = {zeta}', color=colors[idx])

    # Compute and plot the ramp response
    t_ramp, y_ramp = ctrl.forced_response(system, T=t, U=ramp_input)
    axs[2].plot(t_ramp, y_ramp, line_style, label=f'$\\zeta = {zeta}$', color=colors[idx])

# Add labels, legends, grid, and set xlim for all subplots
for ax in axs:
    ax.legend(loc='lower right',fontsize=14); ax.grid(True); ax.set_xlim(0, 25); 
    
# Set plot properties 
axs[0].set_xlim(0, 12); axs[0].set_ylim(-1.1, 1.1); axs[0].set_ylabel('Impulse response')
axs[0].set_title('Impulse, step, and ramp response of 2nd order system')
axs[1].set_xlim(0, 12); axs[1].set_ylim(-0.1, 2.1);  axs[1].set_ylabel('Step response')
axs[2].set_xlim(0, 12); axs[2].set_ylim(0, 12); axs[2].set_xlabel('time $t$');  axs[2].set_ylabel('Ramp response')

# Add arrow with text
arrow_color = '#0055A4'
text_color = arrow_color  # Set text color same as arrow color

axs[0].text(1, 0.5, "increasing $\\zeta$", ha="center", va="center", rotation=-45, size=15, color=text_color, bbox=dict(boxstyle="rarrow,pad=0.3", fc="none", ec=arrow_color, lw=2, alpha=0.5))
axs[1].text(2, 0.75, "increasing $\\zeta$", ha="center", va="center", rotation=-45, size=15, color=text_color, bbox=dict(boxstyle="rarrow,pad=0.3", fc="none", ec=arrow_color, lw=2, alpha=0.5))
axs[2].text(7, 5, "increasing $\\zeta$", ha="center", va="center", rotation=-45, size=15, color=text_color, bbox=dict(boxstyle="rarrow,pad=0.3", fc="none", ec=arrow_color, lw=2, alpha=0.5))

# Save the plot to a PDF file
plt.tight_layout(); plt.savefig('ch05-secondorder-responses.pdf', bbox_inches='tight')
