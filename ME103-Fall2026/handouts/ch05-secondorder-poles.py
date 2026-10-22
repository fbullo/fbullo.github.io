# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import FancyArrowPatch


plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })


# Values of zeta
zeta_values = [0.0, 0.05, 0.1, 0.2, 0.4, 0.5, 0.8, 1.0, 1.1, 1.5, 2.0]
omega_n=1

# Define the background color
bg_color = '#f5f5f5'  # Light gray

# Loop through each zeta value and create a plot for each
for zeta in zeta_values:
    fig, ax = plt.subplots(figsize=(3, 3))  # Create a figure for each zeta value

    # Set up the complex plane with a square aspect ratio
    ax.axhline(0, color='black', lw=1);     ax.axvline(0, color='black', lw=1)
    ax.plot(1, 0, ">k", transform=ax.get_yaxis_transform(), clip_on=False)
    ax.plot(0, 1, "^k", transform=ax.get_xaxis_transform(), clip_on=False)
    ax.set_xlim(-2.7, 0.3);  ax.set_ylim(-1.5, 1.5)
    ax.set_aspect('equal');   ax.set_xticks([]);   ax.set_yticks([])
    for spine in ax.spines.values():
        spine.set_visible(False)

    # Dashed semicircle
    theta = np.linspace(np.pi / 2, 3 * np.pi / 2, 100)
    x = np.cos(theta)
    y = np.sin(theta)
    ax.plot(x, y, 'k--', lw=0.5)

    # Plot poles based on the value of zeta
    if zeta <= 1:
        pole_real = -zeta
        pole_imag = np.sqrt(1 - zeta**2)
        ax.plot(pole_real, pole_imag, 'x', color='#0055A4', markersize=10, markeredgewidth=3)
        ax.plot(pole_real, -pole_imag, 'x', color='#0055A4', markersize=10, markeredgewidth=3)
    else:
        pole1 = -zeta - np.sqrt(zeta**2 - 1)
        pole2 = -zeta + np.sqrt(zeta**2 - 1)
        ax.plot(pole1, 0, 'x', color='#0055A4', markersize=10, markeredgewidth=3)
        ax.plot(pole2, 0, 'x', color='#0055A4', markersize=10, markeredgewidth=3)

    if 0 < zeta < 1:
        # Horizontal lines from each pole to the vertical axis (x=0) with same color and style as semicircle
        ax.hlines(y=pole_imag, xmin=pole_real, xmax=0, colors='black', linestyles='dashed', lw=0.5)
        ax.hlines(y=-pole_imag, xmin=pole_real, xmax=0, colors='black', linestyles='dashed', lw=0.5)
        # Vertical line connecting the two poles
        ax.vlines(x=pole_real, ymin=-pole_imag, ymax=pole_imag, colors='black', linestyles='dashed', lw=0.5)
        
    ax.set_facecolor(bg_color)
    plt.tight_layout()
    tag = f"{zeta:.2f}".replace(".", "p")
    plt.savefig(f"ch05-secondorder-pole-zeta-{tag}.pdf", bbox_inches='tight')
    plt.close(fig)  
