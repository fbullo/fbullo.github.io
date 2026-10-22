# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
import control as ctrl

plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"],
    "text.latex.preamble": r"\usepackage{amsmath}"
})

# Color palette (adjust to your own if needed)
blues = ['#002447', '#003c76', '#0055A4', '#006CD4', '#0085ff', '#239cff', '#58b1ff']

# Transfer function G(s) = 1/(s+1)
G = ctrl.tf([1], [1, 1])

fig, ax = plt.subplots(figsize=(5, 2))

# Compute root locus data without plotting
rlist, klist = ctrl.root_locus(G, plot=False, grid=False)

# Draw axes with arrowheads
ax.axhline(0, color="black", lw=0.4)
ax.axvline(0, color="black", lw=0.4)
ax.plot(1, 0, ">k", transform=ax.get_yaxis_transform(), clip_on=False)  # x-axis arrow
ax.plot(0, 1, "^k", transform=ax.get_xaxis_transform(), clip_on=False)  # y-axis arrow

# Plot the locus manually in your custom color
for branch in rlist.T:   # each column is a branch over K
    ax.plot(branch.real, branch.imag, color=blues[5], lw=2)

# Remove automatic title set by control.root_locus (defensive)
ax.set_title("")

# Fix axis limits so labels stay close to the axis
ax.set_xlim(-4, 1)
ax.set_ylim(-2, 2)

# Remove ticks and enclosing box
ax.set_xticks([])
ax.set_yticks([])
for spine in ax.spines.values():
    spine.set_visible(False)


# (Optional) Axis labels at arrowheads:
# ax.text(1.02, 0.02, r"$\mathrm{Re}(s)$", transform=ax.transAxes, ha="left", va="bottom", fontsize=12)
# ax.text(0.02, 1.02, r"$\mathrm{Im}(s)$", transform=ax.transAxes, ha="left", va="bottom", fontsize=12)

# Pole markers (identical X’s)
ax.plot([-1], [0], marker='x', markersize=10, markeredgewidth=2, color=blues[2])  # open-loop pole
ax.plot([-3], [0], marker='x', markersize=10, markeredgewidth=2, color=blues[2])  # marked closed-loop pole

# Labels just under the poles (use small vertical offset)
offset = -0.2
ax.text(-1, 0 + offset, r"$-\tfrac{1}{\tau}$", ha="center", va="top", fontsize=16)
ax.text(-3, 0 + offset, r"$-\tfrac{1}{\tau_{\text{closed loop}}}$", ha="center", va="top", fontsize=16)

# Arrow from open-loop pole (-1,0) to closed-loop pole (-3,0)
os = .25;
ax.annotate(
    "", 
    xy=(-3+os, 0+os), xycoords="data",        # arrow tip at closed-loop pole
    xytext=(-1-os, 0+os), textcoords="data",  # arrow base at open-loop pole
    arrowprops=dict(
        arrowstyle="->,head_width=0.25,head_length=0.5", 
        color="#666666", 
        lw=3,
        shrinkA=0, shrinkB=0,
        alpha=0.7,
        connectionstyle="arc3,rad=0.2"
    )
)

plt.tight_layout()
plt.savefig("ch07-rootlocus-tauclosedloop.pdf", bbox_inches="tight")
plt.close(fig)
