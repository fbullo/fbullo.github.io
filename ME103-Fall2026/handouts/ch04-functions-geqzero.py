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

def draw_centered_axes_with_arrows(ax, xlim, ylim):
    # Axes lines
    ax.axhline(0, color="black", lw=1)
    ax.axvline(0, color="black", lw=1)

    # Arrowheads
    ax.plot(1, 0, ">k", transform=ax.get_yaxis_transform(), clip_on=False, ms=8)
    ax.plot(0, 1, "^k", transform=ax.get_xaxis_transform(), clip_on=False, ms=8)

    # t label at the end of x-axis
    ax.text(xlim[1], 0.08 * (ylim[1] - ylim[0]), r"$t$", fontsize=24, color="black",
            ha="right", va="bottom")

    # Remove ticks and frame
    ax.set_xticks([]); ax.set_yticks([])
    for spine in ax.spines.values():
        spine.set_visible(False)


# ------------------------ FIGURE 1: GENERAL FUNCTION ------------------------

t1 = np.linspace(-2, 10, 500)
f1 = .5+0.8 * np.exp(-0.1 * t1) * np.sin(0.5 * t1) + 0.2 * t1
f1[t1 < 0] = 0  # Zero out the function for t < 0

fig1, ax1 = plt.subplots(figsize=(8, 3.5))
ax1.plot(t1, f1, color="#006CD4", linewidth=3)

draw_centered_axes_with_arrows(ax1, xlim=(-0.5, 10.5), ylim=(-0.5, 3))

# Label
ax1.text(2.3, 2.5, r"$f(t)$", fontsize=28, color="black")

# Final decorations
ax1.set_xlim(-2., 10.5)
ax1.set_ylim(-0.5, 3)
ax1.set_aspect('auto')
fig1.tight_layout()
fig1.savefig("ch04-functiongeqzero-random.pdf", bbox_inches='tight')


# ------------------------ FIGURE 2: STEP FUNCTION ------------------------

t2 = np.linspace(-2, 10, 500)
u2 = np.heaviside(t2, 1.0)

fig2, ax2 = plt.subplots(figsize=(8, 3.5))
ax2.plot(t2, u2, color="#006CD4", linewidth=4)

draw_centered_axes_with_arrows(ax2, xlim=(-2.2, 10.5), ylim=(-0.2, 1.5))

# Labels
ax2.text(1.7, 1.1, r"$\textbf{1}(t)$", fontsize=28, color="black")

# Final decorations
ax2.set_xlim(-2., 10.5)
ax2.set_ylim(-0.2, 1.5)
ax2.set_aspect('auto')
fig2.tight_layout()
fig2.savefig("ch04-functiongeqzero-step.pdf", bbox_inches='tight')


