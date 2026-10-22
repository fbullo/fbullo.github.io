# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import matplotlib.pyplot as plt
import numpy as np

plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# New pole locations
pole1_real = -1
pole1_imag = 0  # Real pole at -1
pole2_real = -2
pole2_imag = 1  # Complex conjugate poles at -2 ± i

def create_complex_plane_axes():
    """Create and configure the complex plane axes"""
    fig, ax = plt.subplots(figsize=(2, 2))
    
    # Set up the complex plane with updated limits
    ax.axhline(0, color='black', lw=1)  # Horizontal axis
    ax.axvline(0, color='black', lw=1)  # Vertical axis
    ax.set_xlim(-3, 1)
    ax.set_ylim(-2, 2)
    ax.set_aspect('equal')
    ax.tick_params(axis='both', labelsize=9.3)

    # Add arrowheads on axes
    ax.plot(1, 0, ">k", transform=ax.get_yaxis_transform(), clip_on=False)
    ax.plot(0, 1, "^k", transform=ax.get_xaxis_transform(), clip_on=False)
    
    # Set specific xticks and yticks with labels
    ax.set_xticks([pole1_real, pole2_real])
    ax.set_yticks([pole2_imag, -pole2_imag])
    ax.set_xticklabels(['$-1$', '$-2$'])
    ax.set_yticklabels([r'$\mathrm{i}$', r'$-\mathrm{i}$'])
    
    # Remove other ticks
    ax.tick_params(axis='x', which='both', length=0)
    ax.tick_params(axis='y', which='both', length=0)
    
    # Remove the enclosing box
    for spine in ax.spines.values():
        spine.set_visible(False)
    
    return fig, ax

def draw_poles(ax):
    """Draw the poles and reference lines on the given axes"""
    # Plot the poles
    ax.plot(pole1_real, pole1_imag, 'x', color='#0055A4', markersize=10, markeredgewidth=3)  # Single pole at -1
    ax.plot(pole2_real, pole2_imag, 'x', color='#0055A4', markersize=10, markeredgewidth=3)  # Complex pole at -2 + i
    ax.plot(pole2_real, -pole2_imag, 'x', color='#0055A4', markersize=10, markeredgewidth=3)  # Complex pole at -2 - i
    
    # Add dashed lines for complex poles and at -1
    ax.axvline(pole2_real, color='black', linestyle='--', lw=0.5)
    ax.axvline(pole1_real, color='black', linestyle='--', lw=0.5)  # Dashed line at -1
    ax.axhline(pole2_imag, color='black', linestyle='--', lw=0.5)
    ax.axhline(-pole2_imag, color='black', linestyle='--', lw=0.5)

# Command 1: Create the axes
fig, ax = create_complex_plane_axes()

# Command 2: Draw the poles
draw_poles(ax)

# Save the plot to a file
plot_file_path = "ch05-ex-frompolestotf.pdf"
plt.savefig(plot_file_path, bbox_inches='tight')

# Close the figure
plt.close(fig)

