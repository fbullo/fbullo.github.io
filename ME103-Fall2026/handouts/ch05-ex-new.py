# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import matplotlib.pyplot as plt
import numpy as np

def plot_pole_diagram(poles):
    fig, ax = plt.subplots(figsize=(2, 2))

    # Set up the complex plane
    ax.axhline(0, color='black', lw=1)
    ax.axvline(0, color='black', lw=1)
    ax.set_xlim(-5, 1)
    ax.set_ylim(-3, 3)
    ax.set_aspect('equal')

    # Plot the poles and add dashed lines
    for pole in poles:
        ax.plot(pole.real, pole.imag, 'x', color='#0055A4', markersize=10, markeredgewidth=3)
        ax.axvline(pole.real, color='black', linestyle='--', lw=0.5)
        ax.axhline(pole.imag, color='black', linestyle='--', lw=0.5)

    # Set specific xticks and yticks with labels
    ax.set_xticks([pole.real for pole in poles])
    ax.set_yticks([pole.imag for pole in poles])
    ax.set_xticklabels([f'{p.real}' for p in poles])
    ax.set_yticklabels([f'{p.imag}i' for p in poles])

    # Remove other ticks and the enclosing box
    ax.tick_params(axis='x', which='both', length=0)
    ax.tick_params(axis='y', which='both', length=0)
    for spine in ax.spines.values():
        spine.set_visible(False)

    plt.savefig("ch05-ex-frompolestofunction.pdf", bbox_inches='tight')

# Define pole locations
poles = [complex(-3, np.sqrt(2)), complex(-3, -np.sqrt(2))]

# Call the function with the defined poles
plot_pole_diagram(poles)
