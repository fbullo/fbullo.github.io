# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import matplotlib.pyplot as plt
import numpy as np
import control as ctrl

plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })


# Part 1: Frequency response plot for first-order system

# Define the natural frequency and the range of frequency ratios
time_constants = [0.1, .3, .6, 1, 3, 6, 10]
frequencies = np.linspace(0, 10, 1500)
colors = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']

# Plot frequency response for each damping ratio
fig, ax = plt.subplots(figsize=(10, 6))
for idx, tau in enumerate(time_constants):
    frequency_response = 1 / np.sqrt(1 + frequencies**2 * tau**2)
    plt.plot(frequencies, frequency_response, label=f'$\\tau$ = {tau}', color=colors[idx])

# Plot formatting
ax.set_title(r'First-order magnitude response', fontsize=18, pad=10)
ax.set_xlabel(r'Input frequency $\omega$ (rad/s)', fontsize=15)
ax.set_ylabel(r'Magnitude $|G(\mathrm{i}\omega)|$', fontsize=15)
ax.grid(True)
ax.legend(fontsize=12)
ax.tick_params(axis='both', labelsize=12)
ax.set_xlim(0, 8); ax.set_ylim(0, 1.5)

# Add arrow with text
arrow_color = '#0055A4'
ax.text(1, 0.55, "increasing $\\tau$", ha="center", va="center", rotation=45, size=15, color=arrow_color, bbox=dict(boxstyle="larrow,pad=0.3", fc="none", ec=arrow_color, lw=2, alpha=0.5))

# Save the plot to a PDF file
plt.tight_layout()
plt.savefig('ch06-freqresponse-firstorder.pdf', bbox_inches='tight')
plt.clf()

# Part 2: Forced response plots for first-order system

tau = 1
t = np.linspace(0, 50, 1000)  # time range
frequencies = [0.2, 0.5, 1.0, 5.0, 10.0]

for omega in frequencies:
    # Create a first order system
    sys = ctrl.TransferFunction([1], [tau, 1])

    # Define the input signal
    T, input_signal = np.linspace(0, 50, 1000, retstep=True)
    u = np.sin(omega * T)

    # Compute the forced response
    T, yout = ctrl.forced_response(sys, T, u)

    # Plotting
    plt.figure(figsize=(7, 4))
    plt.plot(T, u, label='Input: unit-magnitude sinusoid)', color='#666666', linestyle='--')
    plt.plot(T, yout, label='Forced response of first-order system', color='#0055A4')

    # Plot formatting
    plt.title(rf'Forced Response of a First Order System to $\sin({omega}t)$')
    plt.xlabel('Time (t)')
    plt.ylabel('Response Amplitude')
    plt.grid(True)
    plt.legend()
    plt.xlim(0, max(T)); plt.ylim(-1.5, 1.5)

    # Save the plot to a PDF file
    plt.tight_layout()
    tag = f"{omega:.1f}".replace(".", "p")
    plt.savefig(f'ch06-freqresponse-firstorder-omega-{tag}.pdf', bbox_inches='tight')
    plt.clf()

# Part 3: Bode magnitude plot for a first-order system

# Define the time constant
tau = 1

# Create the transfer function of the first-order system
sys = ctrl.TransferFunction([1], [tau, 1])

# Generate Bode plot
plt.figure()
ctrl.bode_plot(sys, dB=True, title="")

# Save the Bode plot to a PDF file
plt.tight_layout()
plt.savefig('ch06-freqresponse-firstorder-bode.pdf', bbox_inches='tight')
plt.clf()

plt.clf()
