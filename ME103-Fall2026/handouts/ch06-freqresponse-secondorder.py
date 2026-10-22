# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import matplotlib.pyplot as plt
import numpy as np
import control as ctrl

plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"]
})

# Part 1: Magnitude Frequency Response for Second Order System

# Natural frequency (normalized plot)
natural_frequency = 1.0
frequency_ratios = np.linspace(0, 3, 500)

damping_ratios = [
    0.01,
    0.15,
    0.3,
    0.6,
    1/np.sqrt(2),
    (1/np.sqrt(2) + 1)/2,
    1.0,
    1.5,
    3.0
]

# Colors (9 entries for 9 damping ratios)
colors = [
    '#471b00',
    '#752d00',
    '#a43e00',
    '#d35000',
    '#ff6100',
    '#0055A4',
    '#006CD4',
    '#0085ff',
    '#58b1ff',
]

# Plot frequency response for each damping ratio
fig, ax = plt.subplots(figsize=(10, 6))
for idx, zeta in enumerate(damping_ratios):
    frequency_response = 1 / np.sqrt(
        (1 - frequency_ratios**2)**2 + (2 * zeta * frequency_ratios)**2
    )
    # dashed line for zeta = 1
    line_style = '--' if np.isclose(zeta, 1.0) else '-'
    ax.plot(
        frequency_ratios,
        frequency_response,
        line_style,
        label=rf'$\zeta = {zeta:.2g}$',
        color=colors[idx]
    )

# Plot formatting
ax.set_title(r'Second-order magnitude response', fontsize=18, pad=10)
ax.set_xlabel(r'Frequency ratio $\omega/\omega_n$', fontsize=15)
ax.set_ylabel(r'Magnitude $|G(\mathrm{i}\omega)|$', fontsize=15)
ax.grid(True)
ax.legend(fontsize=11)
ax.tick_params(axis='both', labelsize=12)
ax.set_xlim(0, 3)
ax.set_ylim(0, 5)

# Add arrow with text
arrow_color = '#0055A4'
ax.text(
    1.5, 0.85,
    r'increasing $\zeta$',
    ha="center", va="center",
    rotation=45, size=15, color=arrow_color,
    bbox=dict(
        boxstyle="larrow,pad=0.3",
        fc="none", ec=arrow_color,
        lw=2, alpha=0.5
    )
)

# --- Resonance markers -----------------------------------
for idx, zeta in enumerate(damping_ratios):
    if 0 <= zeta < 1/np.sqrt(2):
        omega_r = natural_frequency * np.sqrt(1 - 2*zeta**2)
        M_r = 1 / (2 * zeta * np.sqrt(1 - zeta**2))

        ax.plot(
            omega_r / natural_frequency,
            M_r,
            'o',
            color=colors[idx],
            markersize=8,
            markeredgecolor='black'
        )
# ----------------------------------------------------------

# Vertical line at omega/omega_n = 1
ax.axvline(x=1, color='#666666', linestyle='--')

fig.tight_layout()
fig.savefig('ch06-freqresponse-secondorder.pdf', bbox_inches='tight')
plt.clf()

# Part 2: Bode Plot for Second Order System with Varying Zeta

omegan = 1.0

# Frequency grid for Bode plot
omega = np.logspace(-1, 1, 500)  # 0.1 to 10 rad/s

fig, (mag_ax, phase_ax) = plt.subplots(2, 1, figsize=(10, 12))

for idx, zeta in enumerate(damping_ratios):
    sys = ctrl.TransferFunction([omegan**2], [1, 2 * zeta * omegan, omegan**2])
    mag, phase, _ = ctrl.bode_plot(
        sys, omega=omega, dB=False, plot=False
    )
    # mag is in abs; convert to dB manually
    mag_db = 20 * np.log10(mag)
    mag_ax.semilogx(omega, mag_db,
                    label=rf'$\zeta = {zeta:.2g}$',
                    color=colors[idx])
    phase_ax.semilogx(omega, phase * (180 / np.pi),
                      color=colors[idx])

    
# Magnitude subplot formatting
mag_ax.set_title(r'Bode magnitude')
mag_ax.set_ylabel(r'Magnitude (dB)')
mag_ax.legend(loc='upper right')
mag_ax.grid(True, which='both')

# Phase subplot formatting
phase_ax.set_title(r'Bode phase')
phase_ax.set_xlabel(r'Frequency $\omega$ (rad/s)')
phase_ax.set_ylabel(r'Phase (degrees)')
phase_ax.grid(True, which='both')

plt.suptitle(
    r'Bode plots for second-order system with varying damping ratios $\zeta$'
)
plt.tight_layout(rect=[0, 0.03, 1, 0.95])
plt.savefig('ch06-freqresponse-secondorder-bode.pdf', bbox_inches='tight')
plt.clf()
