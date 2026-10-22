# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
import control as ctrl
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Define the range of proportional gains
Kp_values = [1]

# Define brown-like color range
colors = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']

# Frequency vector for Bode plot
frequency_vector = np.logspace(-3, 3, 1000)  # From 10^-3 to 10^3 rad/s

# Time vector for step response
time_vector = np.linspace(0, 20, 1000)  # From 0 to 20 seconds

# Define the unstable system G(s) = 1 / (tau * s + 1)
tfP = ctrl.TransferFunction([1], [1, -1])

# Set up the figure for Bode and step response plots
plt.figure(figsize=(18, 12))

for i, title in enumerate(["Sensitivity Y/N", "Complementary Sensitivity -X/N", "Load->State X/D Sensitivity", "Noise->Control U/N Sensitivity"]):
    for Kp_idx, Kp in enumerate(Kp_values):
        # Define the zero-pole cancelation controller Kp - Ki/s
        tfPI = ctrl.TransferFunction([1, -1], [1, 0])

        color = colors[Kp_idx % len(colors)]

        # Compute the transfer functions
        tfL = tfP * tfPI  # Open-loop transfer function        
        tfL = ctrl.TransferFunction([1], [1, 0]) ## tfP * tfPI  # Open-loop transfer function
        
        tfS = 1 / (1 + tfL)  # Sensitivity 
        tfT = tfL / (1 + tfL)  # Complementary sensitivity
        
        tfCS = tfPI / (1 + tfL)  # Noise->Control Sensitivity
        tfCS = ctrl.TransferFunction([1, -1], [1, 1]) 
                
        tfLS = tfP / (1 + tfL)  # Load->State Sensitivity

        # Select the appropriate transfer function
        func = {'Sensitivity Y/N': tfS, 'Complementary Sensitivity -X/N': tfT, 'Load->State X/D Sensitivity': tfLS,
                'Noise->Control U/N Sensitivity': tfCS}[title]
        
        # Step response plot setup
        plt.subplot(4, 2, 2 * i + 1)

                # Compute and plot the step response of the transfer function
        T, yout = ctrl.step_response(func,  T=time_vector)
        plt.plot(T, yout, label=f'Kp={Kp}', color=color)
        plt.xlabel('Time [s]')
        plt.ylabel('Response')
        plt.title(f'Step Response: {title}')
        plt.grid();  plt.xlim(0,20)

        # Bode plot setup
        plt.subplot(4, 2, 2 * i + 2)
        
        # Plot Bode plot of the transfer function
        mag, phase, omega = ctrl.bode_plot(func, dB=True, omega=frequency_vector, plot=False)
        plt.semilogx(omega, 20 * np.log10(mag), label=f'Kp={Kp}', color=color)
        plt.xlabel('Frequency [rad/s]')
        plt.ylabel('Magnitude [dB]')
        plt.title(f'Bode: {title}')
        plt.grid(which='both')


plt.tight_layout()
plt.savefig('ch08-gangoffour-cancelation.pdf')

# how do i add code that prints the 4 transfer functions in latex?
