# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
import control as ctrl

plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"]})

# System parameters
natural_frequency = 1.0  # Natural frequency of the system (omega_n)

# Time parameters
t = np.linspace(0, 300, 2000)  # Time vector

# Beta values - ratio of input frequency to natural frequency
betas = [0.01, 0.1, 0.5, 0.95, 1.0, 1.025, 1.5, 3.0, 9.0]

# File paths for saving the plots
file_paths_undamped = []
file_paths_lightly_damped = []

# Function to generate and save plots
def generate_plots(damping_ratio, file_prefix):
    # Create a second-order system
    num = [natural_frequency**2]  # Numerator coefficients
    den = [1, 2 * damping_ratio * natural_frequency, natural_frequency**2]  # Denominator coefficients
    system = ctrl.TransferFunction(num, den)

    file_paths = []
    for beta in betas:
        input_frequency = beta * natural_frequency  # Input frequency (omega)
        # Sinusoidal input
        input_signal = np.sin(input_frequency * t)
        # Compute forced response
        t_out, response = ctrl.forced_response(system, T=t, U=input_signal)

        # Plotting
        plt.figure(figsize=(10, 6))
        plt.plot(t_out, response, label='System Response')
        plt.plot(t_out, input_signal, color='#cccccc', label='Input Signal')
        plt.title(f'Response of a {"Lightly Damped" if damping_ratio > 0 else "Undamped"} System to a Sinusoidal Input ($\\omega/\\omega_{{\\mathrm{{n}}}}$ = {beta}, $\\zeta = {damping_ratio}$)')
        plt.xlabel('Time')
        plt.ylabel('System Response')
        plt.grid(True)
        plt.xlim(0, 300)
        plt.ylim(-25, 25)  # Fixed y-axis range for all plots
        plt.legend()

        # Save the plot to a PDF file
        tag = f"{beta:.3f}".replace(".", "p")
        file_name = f"{file_prefix}-beta-{tag}.pdf"
        plt.savefig(file_name, bbox_inches='tight')
        file_paths.append(file_name)

        plt.close()
    
    return file_paths

# Generate plots for undamped system
generate_plots(0.0, "ch06-freqresponse-undamped")

# Generate plots for lightly damped system
generate_plots(0.001, "ch06-freqresponse-lightlydamped")

# Generate plots for lightly damped system
generate_plots(0.01, "ch06-freqresponse-damped")
