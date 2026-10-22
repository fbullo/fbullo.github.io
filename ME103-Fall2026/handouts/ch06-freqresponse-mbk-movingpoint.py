# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import matplotlib.pyplot as plt
import numpy as np
import control as ctrl

plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Define the natural frequency and the range of frequency ratios
natural_frequency = 1.0  # Natural frequency (normalized plot)
frequency_ratios = np.linspace(0, 3, 500)  # Frequency ratio range

# Define a range of damping ratios
damping_ratios = [0.0, 0.15, 0.3, 0.6, 1.0, 1.5, 3.0]  # Damping ratios, zeta
colors = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']

# Frequency Response Plot
plt.figure(figsize=(10, 6))
for zeta, color in zip(damping_ratios, colors):
    frequency_response = np.sqrt((1 + (2 * zeta * frequency_ratios)**2) /
                                 ((1 - frequency_ratios**2)**2 + (2 * zeta * frequency_ratios)**2))
    plt.plot(frequency_ratios, frequency_response, label=f'$\\zeta$ = {zeta}', color=color)

# Plot formatting for frequency response
plt.title('Magnitude Frequency Response Curves for Mass-Spring-Damper with Moving Point')
plt.xlabel('Frequency Ratio (frequency / natural frequency)')
plt.ylabel('Magnitude Frequency Response')
plt.grid(True)
plt.legend()
plt.ylim(0, 5)  # Limit the y-axis for better visibility

# Adding critical vertical lines and highlighted points
plt.axvline(x=1, color='#666666', linestyle='--')  # Frequency ratio = 1
plt.axvline(x=np.sqrt(2), color='#666666', linestyle='--')  # Frequency ratio = √2
plt.scatter(np.sqrt(2), 1, color='black', s=50, zorder=5)  # Highlight specific point (√2, 1)
plt.text(1.55, 1.2, r'$(\sqrt{2},1)$', fontsize=14, ha='center', va='center',
         bbox=dict(facecolor='white', edgecolor='none', pad=0.5))

# Axis limits for frequency response plot
plt.xlim(0, 3)
plt.ylim(0, 5)

# Save the frequency response plot
plt.tight_layout()
plt.savefig('ch06-freqresponse-mbk-movingpoint.pdf', bbox_inches='tight')


# Step Response Plot for Damping Ratio ζ = 0.3
zeta_step = 0.3  # Damping ratio for the step response
numerator = [2 * zeta_step * natural_frequency, natural_frequency**2]  # Numerator of transfer function
denominator = [1, 2 * zeta_step * natural_frequency, natural_frequency**2]  # Denominator for second-order system

# Create transfer function
sys_tf = ctrl.TransferFunction(numerator, denominator)

# Compute step response
time, step_response = ctrl.step_response(sys_tf)

# Plot step response
plt.figure(figsize=(10, 6))
plt.plot(time, step_response, label=f'Step Response ($\\zeta$ = {zeta_step})', color='#0055A4')
plt.title('Step Response of Mass-Spring-Damper System')
plt.xlabel('time $t$')
plt.ylabel('response')
plt.grid(True)
plt.legend()
plt.show()
