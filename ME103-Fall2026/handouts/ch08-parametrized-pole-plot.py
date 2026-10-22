# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

"""Parametrized closed-loop pole plot for Chapter 8, Exercise 8.6."""

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np


plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"],
    "text.latex.preamble": r"\usepackage{amsmath,amssymb,amsfonts}",
    "axes.labelsize": 12,
    "xtick.labelsize": 10,
    "ytick.labelsize": 10,
    "legend.fontsize": 9,
})

blues = [
    "#002447", "#003c76", "#0055A4", "#006CD4",
    "#0085ff", "#239cff", "#58b1ff",
]
oranges = [
    "#471b00", "#752d00", "#a43e00", "#d35000",
    "#ff6100", "#ff7f1a", "#ff9b56",
]
grays = [
    "#1a1a1a", "#333333", "#4d4d4d", "#666666",
    "#999999", "#cccccc", "#e6e6e6", "#f5f5f5",
]

zeta = 0.4
tau = 1.0
fast_time_constant = 0.1
omega_threshold = 8.8
omega_stable = 6.0
omega_unstable = 11.0


def poles(omega_n):
    """Roots of tau*T*s^3 + (T+tau)*s^2 + 2*zeta*tau*omega_n*s
    + tau*omega_n^2.
    """
    coefficients = [
        tau * fast_time_constant,
        fast_time_constant + tau,
        2.0 * zeta * tau * omega_n,
        tau * omega_n**2,
    ]
    return np.roots(coefficients)


def ordered_poles(omega_n):
    """Return the lower, real, and upper roots in a consistent order."""
    roots = poles(omega_n)
    real_index = np.argmin(np.abs(roots.imag))
    real_root = roots[real_index]
    pair = np.delete(roots, real_index)
    lower_root = pair[np.argmin(pair.imag)]
    upper_root = pair[np.argmax(pair.imag)]
    return np.array([lower_root, real_root, upper_root])


omega_values = np.linspace(1.5, 12.5, 600)
pole_branches = np.array([ordered_poles(value) for value in omega_values])

stable_poles = ordered_poles(omega_stable)
threshold_poles = ordered_poles(omega_threshold)
unstable_poles = ordered_poles(omega_unstable)

fig, ax = plt.subplots(figsize=(7.0, 5.2))
ax.axvspan(0.0, 2.0, color=oranges[6], alpha=0.16, zorder=0)
ax.axvline(0.0, color="black", lw=0.9)
ax.axhline(0.0, color="black", lw=0.7)

ax.plot(
    pole_branches[:, 0].real, pole_branches[:, 0].imag,
    color=blues[2], lw=1.8, label="complex-conjugate pole paths",
)
ax.plot(
    pole_branches[:, 2].real, pole_branches[:, 2].imag,
    color=blues[2], lw=1.8,
)
ax.plot(
    pole_branches[:, 1].real, pole_branches[:, 1].imag,
    color=grays[2], lw=1.8, label="real-pole path",
)


def add_direction_arrow(branch_index, omega_location, color):
    index = np.searchsorted(omega_values, omega_location)
    start = pole_branches[index, branch_index]
    end = pole_branches[index + 12, branch_index]
    ax.annotate(
        "", xy=(end.real, end.imag), xytext=(start.real, start.imag),
        arrowprops={"arrowstyle": "-|>", "color": color, "lw": 1.8},
    )


add_direction_arrow(0, 7.0, blues[2])
add_direction_arrow(1, 7.0, grays[2])
add_direction_arrow(2, 7.0, blues[2])

ax.scatter(
    stable_poles.real, stable_poles.imag, s=52, marker="o",
    facecolor=blues[5], edgecolor=blues[0], linewidth=0.8, zorder=4,
    label=r"stable: $\omega_n=6$",
)
ax.scatter(
    threshold_poles.real, threshold_poles.imag, s=58, marker="D",
    facecolor="black", edgecolor="black", linewidth=0.8, zorder=5,
    label=r"threshold: $\omega_n=8.8$",
)
ax.scatter(
    unstable_poles.real, unstable_poles.imag, s=62, marker="X",
    facecolor=oranges[4], edgecolor=oranges[0], linewidth=0.8, zorder=5,
    label=r"unstable: $\omega_n=11$",
)

ax.text(
    0.12, 12.1, "unstable half-plane", color=oranges[2],
    ha="left", va="top", fontsize=10,
)
ax.set_xlim(-14.0, 2.0)
ax.set_ylim(-13.0, 13.0)
ax.set_xlabel(r"$\operatorname{Re}(s)$")
ax.set_ylabel(r"$\operatorname{Im}(s)$")
ax.grid(True, color=grays[6], linewidth=0.7)
ax.legend(loc="upper left", frameon=True, fancybox=False, ncol=1)
fig.tight_layout()

output_path = Path(__file__).with_suffix(".pdf")
fig.savefig(output_path, bbox_inches="tight")
print(f"Saved {output_path}")
print(f"stable poles (omega_n={omega_stable:g}): {poles(omega_stable)}")
print(f"threshold poles (omega_n={omega_threshold:g}): {poles(omega_threshold)}")
print(f"unstable poles (omega_n={omega_unstable:g}): {poles(omega_unstable)}")
