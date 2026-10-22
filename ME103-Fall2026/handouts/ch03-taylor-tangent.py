# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

"""Illustrate a scalar first-order Taylor approximation for Chapter 3."""

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np


def f(x):
    return np.log1p(3 * x)


x_star = 0.65
x_sample = 1.65
f_star = f(x_star)
slope = 3 / (1 + 3 * x_star)


def tangent(x):
    return f_star + slope * (x - x_star)


plt.rcParams.update({
    "font.family": "serif",
    "font.size": 12,
    "mathtext.fontset": "cm",
    "pdf.fonttype": 42,
    "text.usetex": True,
    "text.latex.preamble": r"\usepackage{amsmath, amsfonts, amssymb}" 
})

fig, ax = plt.subplots(figsize=(7.0, 3.7))
grid = np.linspace(0, 2.7, 400)
tangent_grid = np.linspace(0.2, 2.35, 200)

ax.plot(grid, f(grid), color="#0055A4", linewidth=2.5,
        label=r"nonlinear function $f$")
ax.plot(tangent_grid, tangent(tangent_grid), color="#A43E00",
        linewidth=2.2, linestyle="--",
        label=r"tangent at $x^*$: slope $f'(x^*)$")

ax.scatter([x_star], [f_star], color="black", s=45, zorder=5)
ax.scatter([x_sample], [f(x_sample)], color="#0055A4", s=45, zorder=5)
ax.scatter([x_sample], [tangent(x_sample)], color="#A43E00", s=45, zorder=5)

ax.vlines(x_star, 0, f_star, color="#999999", linestyles=":", linewidth=1.3)
ax.vlines(x_sample, 0, tangent(x_sample), color="#999999", linestyles=":",
          linewidth=1.3)
ax.axhline(0, color="#4d4d4d", linewidth=1)

ax.annotate("", xy=(x_star, -0.27), xytext=(x_sample, -0.27),
            arrowprops={"arrowstyle": "<->", "color": "black", "lw": 1.2})
ax.text((x_star + x_sample) / 2, -0.34, r"$\delta x=x-x^*$",
        ha="center", va="top")
ax.annotate(r"$f(x^*)$", xy=(x_star, f_star), xytext=(0.25, 1.45),
            arrowprops={"arrowstyle": "-", "color": "0.3", "lw": 1},
            ha="center")

ax.set_xlim(0, 2.7)
ax.set_ylim(-0.60, 3.0)
ax.set_xticks([x_star, x_sample], [r"$x^*$", r"$x$"])
ax.set_yticks([])
ax.legend(loc="upper left", frameon=False, fontsize=10.5)
ax.spines[["top", "right", "left"]].set_visible(False)
ax.spines["bottom"].set_position(("data", 0))
ax.tick_params(axis="x", length=0, pad=6)

fig.tight_layout()
fig.savefig(Path(__file__).with_suffix(".pdf"), bbox_inches="tight")
plt.close(fig)
