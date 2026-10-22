# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.patches import FancyArrowPatch
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Set up parameters for the plot
zeta = 0.7
omega_n = 1  # Natural frequency
bg_color = '#f5f5f5'  # Light gray background

fig, ax = plt.subplots(figsize=(3, 3))

# Set up the complex plane with a square aspect ratio
ax.axhline(0, color='black', lw=1)
ax.axvline(0, color='black', lw=1)
ax.plot(1, 0, ">k", transform=ax.get_yaxis_transform(), clip_on=False)
ax.plot(0, 1, "^k", transform=ax.get_xaxis_transform(), clip_on=False)
ax.set_xlim(-1.3, 1.3)
ax.set_ylim(-1.1, 1.1)
ax.set_aspect('equal')
ax.set_xticks([])
ax.set_yticks([])
for spine in ax.spines.values():
    spine.set_visible(False)

# Dashed semicircle
theta = np.linspace(np.pi / 2, 3 * np.pi / 2, 100)
x = np.cos(theta)
y = np.sin(theta)
ax.plot(x, y, 'k--', lw=0.5)

# Pole position
pole_real = -zeta * omega_n
pole_imag = np.sqrt(1 - zeta**2) * omega_n
ax.plot(pole_real, pole_imag, 'x', color='#0055A4', markersize=8, markeredgewidth=2)
ax.plot(pole_real, -pole_imag, 'x', color='#0055A4', markersize=8, markeredgewidth=2)


# Small tick marks
tick_length = 0.05  # Reduced tick length

# Tick at +i omega_n
ax.plot([0, tick_length], [omega_n, omega_n], color='black', lw=0.8)
ax.text(0.1, omega_n, r'$+\mathrm{i} \omega_{\mathrm{n}}$', va='center', ha='left', fontsize=8)

# Tick at +i omega_d
ax.plot([0, tick_length], [pole_imag, pole_imag], color='black', lw=0.8)
ax.text(0.1, pole_imag, r'$+\mathrm{i} \omega_{\mathrm{d}} = +\mathrm{i}\omega_{\mathrm{n}}\sqrt{1-\zeta^2}$', va='center', ha='left', fontsize=8)

# Tick at -zeta * omega_n on the real axis
ax.plot([pole_real, pole_real], [0, -tick_length], color='black', lw=0.8)
ax.text(pole_real, -0.075, r'$-\zeta \omega_{\mathrm{n}}$', va='top', ha='center', fontsize=8)

# Tick at -omega_n (negative real number)
ax.plot([-omega_n, -omega_n], [0, -tick_length], color='black', lw=0.8)
ax.text(-omega_n, -0.02, r'$-\omega_{\mathrm{n}}$', va='top', ha='right', fontsize=8)

# Tick at -i omega_n
ax.plot([0, tick_length], [-omega_n, -omega_n], color='black', lw=0.8)
ax.text(0.1, -omega_n, r'$-\mathrm{i} \omega_{\mathrm{n}}$', va='center', ha='left', fontsize=8)


# Dashed lines connecting axes to the pole
# Vertical dashed line from the real axis to +i omega_d
ax.plot([pole_real, pole_real], [0, pole_imag], 'k--', lw=0.5)

# Horizontal dashed line from the imaginary axis to the pole
ax.plot([0, pole_real], [pole_imag, pole_imag], 'k--', lw=0.5)

# Arc with clockwise arrow at radius = 0.5 from the origin to the pole
arc_radius = 0.5
arc_theta = np.linspace(np.pi, np.arctan2(pole_imag, pole_real), 100)
arc_x = arc_radius * np.cos(arc_theta)
arc_y = arc_radius * np.sin(arc_theta)
ax.plot(arc_x, arc_y, 'k--', lw=0.5)

# Add an arrow to the arc to indicate clockwise direction
arrow = FancyArrowPatch(
    posA=(arc_radius * np.cos(arc_theta[-1]+0.15), arc_radius * np.sin(arc_theta[-1]+0.15)),
    posB=(arc_radius * np.cos(arc_theta[-1]+0.075), arc_radius * np.sin(arc_theta[-1]+0.075)),
    color="black",
    arrowstyle='-|>',
    mutation_scale=7,
)
ax.add_patch(arrow)

# Label to the right of the end of the arc
end_arc_x = arc_radius * np.cos(arc_theta[-1])
end_arc_y = arc_radius * np.sin(arc_theta[-1])
ax.text(end_arc_x-0.1, end_arc_y-0.25, r'$\arccos(\zeta)$', fontsize=8, ha='left', va='center')

# Dashed line segment from the origin to the pole
ax.plot([0, pole_real], [0, pole_imag], 'k--', lw=0.5)

# Add title to the plot in LaTeX
# ax.set_title(r"Poles of an underdamped system in the complex plane", fontsize=10, pad=10)

# Set background color and save figure
ax.set_facecolor(bg_color)
plt.tight_layout()
plt.savefig("ch05-secondorder-pole-beta.pdf", bbox_inches='tight')
plt.close(fig)
