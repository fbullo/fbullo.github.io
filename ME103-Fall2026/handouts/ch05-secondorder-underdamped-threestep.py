# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import matplotlib.pyplot as plt
import numpy as np
import control as ctrl

# Configure LaTeX style for plots
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"]})
blues = ['#002447', '#0055A4', '#239cff', '#58b1ff']

# Define the system's denominator (common for all three step responses)
zeta_step = 0.4  # Damping ratio
natural_frequency = 1.0  # Natural frequency
denominator = [1, 2 * zeta_step * natural_frequency, natural_frequency**2]  # Denominator for second-order system

# Define the numerators for the three systems
numerators = [
    [1],        # Numerator for first system
    [1, 0],     # Numerator for second system
    [1, 1],     # Numerator for third system
    [-1, 1]     # Numerator for fourth system
]

# Create labels for each step response
labels = [
    'no zero: $\\displaystyle G_1(s) = \\frac{1}{s^2 + .8 s + 1}$',
    'zero at origin: $\\displaystyle G_2(s) = \\frac{s}{s^2 + .8 s + 1}$',
    'left half-plane zero: $\\displaystyle G_3(s) = \\frac{s+1}{s^2 + .8 s + 1}$',
    'right half-plane zero: $\\displaystyle G_4(s) = \\frac{-s+1}{s^2 + .8 s + 1}$'
]

# Initialize the plot
plt.figure(figsize=(10, 6))

# Define a custom time vector with higher resolution
time = np.linspace(0, 12, 1000)  # 0 to 12 seconds with 1000 points

# Loop through each numerator to calculate and plot the step response
for i, (numerator, label, color) in enumerate(zip(numerators, labels, blues[:4])):
    # Create transfer function
    sys_tf = ctrl.TransferFunction(numerator, denominator)
    
    # Compute step response
    time, step_response = ctrl.step_response(sys_tf, T=time)
    
    # Plot the step response with a dashed line for the first system
    line_style = '--' if i == 0 else '-'
    plt.plot(time, step_response, label=label, color=color, linestyle=line_style)

# Formatting the plot
plt.title('Step responses of underdamped second-order systems with different numerators')
plt.xlabel('time $t$')
plt.ylabel('response')
plt.xlim(0, 10)
plt.ylim(-0.5, 2)
plt.grid(True)
plt.legend()

# Save the plot to a PDF file
plt.savefig('ch05-secondorder-underdamped-threestep.pdf')
plt.close()
