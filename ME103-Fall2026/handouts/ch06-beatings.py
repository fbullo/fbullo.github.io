# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt

plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"]
})

# Parameters
f1 = 20
f2 = 20.2
Tfinal = 12
omega1 = np.pi * f1
omega2 = np.pi * f2
t = np.linspace(0, Tfinal, 100000)

# Signals
y1 = np.sin(omega1 * t)
y2 = np.sin(omega2 * t)
y_sum = y1 + y2

# Color palettes
shades_of_blue = ['#002447', '#003c76', '#0055A4', '#006CD4', '#0085ff', '#239cff', '#58b1ff']
shades_of_orange = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']

# Reusable plotting function
def save_plot(t, y_list, labels, colors, filename, title=None, font_scale=1.0, legend_bottom=False):
    plt.figure(figsize=(12, 4))
    for y, label, color in zip(y_list, labels, colors):
        plt.plot(t, y, label=label, color=color, linewidth=1.5)
    plt.xlim(0, t[-1])
    plt.xlabel(r"time $t$", fontsize=10*font_scale)
    plt.ylabel(r"amplitude", fontsize=10*font_scale)
    if title:
        plt.title(title, fontsize=11*font_scale)

    ax = plt.gca()
    ax.tick_params(axis='both', which='major', labelsize=9*font_scale)
    plt.grid(True)

    if legend_bottom:
        plt.legend(loc='lower center', bbox_to_anchor=(0.5, -0.25), ncol=3, fontsize=9*font_scale, frameon=True)
        plt.subplots_adjust(bottom=0.3)
    else:
        plt.legend(fontsize=9*font_scale)

    plt.tight_layout()
    plt.savefig(filename, bbox_inches='tight')
    plt.close()

# Plot 1: First sinusoid alone (double fonts, legend center-bottom)
save_plot(
    t, [y1],
    [r"$\sin(\omega_1 t)$"],
    [shades_of_orange[2]],
    "ch06-beatings-first-sinusoid.pdf",
    font_scale=2.0,
    legend_bottom=True
)

# Plot 2: Second sinusoid alone (double fonts, legend center-bottom)
save_plot(
    t, [y2],
    [r"$\sin(\omega_2 t)$"],
    [shades_of_orange[5]],
    "ch06-beatings-second-sinusoid.pdf",
    font_scale=2.0,
    legend_bottom=True
)

# Plot 3: Both sinusoids (not summed)
save_plot(
    t, [y1, y2],
    [r"$\sin(\omega_1 t)$", r"$\sin(\omega_2 t)$"],
    [shades_of_orange[2], shades_of_orange[5]],
    "ch06-beatings-two-sinusoids.pdf",
    font_scale=2.0,
    legend_bottom=True
)

# Plot 4: Sum of both sinusoids (double fonts, legend center-bottom)
save_plot(
    t, [y_sum],
    [r"$\sin(\omega_1 t) + \sin(\omega_2 t)$"],
    [shades_of_blue[5]],
    "ch06-beatings-sum-sinusoids.pdf",
    font_scale=2.0,
    legend_bottom=True
)

# Plot 5: First sinusoid, only 2 full periods
T_period1 = 2 * np.pi / omega1
t_short = np.linspace(0, 2 * T_period1, 2000)
y1_short = np.sin(omega1 * t_short)
save_plot(
    t_short, [y1_short],
    [r"$\sin(\omega_1 t)$"],
    [shades_of_orange[2]],
    "ch06-beatings-first-sinusoid-2periods.pdf"
)

# Plot 5b: First sinusoid, only 4 full periods
T_period1 = 4 * np.pi / omega1
t_short = np.linspace(0, 2 * T_period1, 2000)
y1_short = np.sin(omega1 * t_short)
save_plot(
    t_short, [y1_short],
    [r"$\sin(\omega_1 t)$"],
    [shades_of_orange[2]],
    "ch06-beatings-first-sinusoid-4periods.pdf"
)

# Plot 6: All three together (double fonts, legend center-bottom)
save_plot(
    t, [y1, y2, y_sum],
    [r"$\sin(\omega_1 t)$", r"$\sin(\omega_2 t)$", r"$\sin(\omega_1 t) + \sin(\omega_2 t)$"],
    [shades_of_orange[2], shades_of_orange[5], shades_of_blue[5]],
    "ch06-beatings.pdf",
    font_scale=2.0,
    legend_bottom=True
)

# --- Stylized Plots: Input and Output ---

# Input signal: base sinusoid
y_input = np.sin(omega1 * t_short)

# Output signal: scaled and phase-shifted
y_output = 3 * np.sin(omega1 * t_short + np.pi / 4)

# Common color
blue = shades_of_blue[3]

# Stylized input plot
plt.figure(figsize=(6, 2.5))
plt.plot(t_short, y_input, color=blue, linewidth=2)
plt.axis('off')
plt.xlim(t_short[0], t_short[-1])
plt.ylim(-3.2, 3.2)
plt.tight_layout()
plt.savefig("ch06-beatings-input.pdf", bbox_inches='tight')
plt.close()

# Stylized output plot
plt.figure(figsize=(6, 2.5))
plt.plot(t_short, y_output, color=blue, linewidth=2)
plt.axis('off')
plt.xlim(t_short[0], t_short[-1])
plt.ylim(-3.2, 3.2)
plt.tight_layout()
plt.savefig("ch06-beatings-output.pdf", bbox_inches='tight')
plt.close()

# Extended time for double the duration
Tfinal2 = 2 * Tfinal
t_long = np.linspace(0, Tfinal2, 200000)
y_sum_long = np.sin(omega1 * t_long) + np.sin(omega2 * t_long)

save_plot(
    t_long, [y_sum_long],
    [r"$\sin(\omega_1 t) + \sin(\omega_2 t)$"],
    [shades_of_blue[5]],
    "ch06-beatings-sum-sinusoids-long.pdf"
)


