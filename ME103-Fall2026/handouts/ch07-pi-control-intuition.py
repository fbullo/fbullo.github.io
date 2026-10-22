# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt


plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"],
})

BLUE = "#239cff"
ORANGE = "#ff9b56"
GRAY = "#666666"


def draw_axes(axis):
    axis.set_xlim(0, 4.8)
    axis.set_ylim(-0.04, 1.05)
    axis.set_xticks([])
    axis.set_yticks([])
    axis.spines[["top", "right", "left", "bottom"]].set_visible(False)
    axis.annotate(
        "", xy=(4.75, 0), xytext=(0, 0),
        arrowprops={"arrowstyle": "->", "color": GRAY, "linewidth": 1.0},
    )
    axis.annotate(
        "", xy=(0, 1.02), xytext=(0, 0),
        arrowprops={"arrowstyle": "->", "color": GRAY, "linewidth": 1.0},
    )
    axis.text(4.73, -0.035, r"$t$", color=GRAY, fontsize=15,
              ha="right", va="top")
    axis.text(-0.04, 1.0, r"$e(t)$", color=GRAY, fontsize=15,
              ha="right", va="top")


time = np.linspace(0, 4.6, 500)
error = 0.78 * np.exp(-0.90 * time) + 0.15

figure, axes = plt.subplots(1, 2, figsize=(8.2, 3.0), sharey=True)

draw_axes(axes[0])
axes[0].plot(time, error, color=BLUE, linewidth=2.4)
axes[0].axhline(error[-1], color=GRAY, linestyle=":", linewidth=1.0)
axes[0].text(.08, .93, r"$u_P(t)=k_P e(t)$", transform=axes[0].transAxes,
             fontsize=15, ha="left", va="top")
axes[0].annotate(
    "", xy=(3.0, error[np.searchsorted(time, 3.0)]), xytext=(3.0, .62),
    arrowprops={"arrowstyle": "->", "color": GRAY, "linewidth": 1.0},
)

draw_axes(axes[1])
axes[1].fill_between(time, 0, error, color=ORANGE, alpha=.18)
axes[1].plot(time, error, color=BLUE, linewidth=2.4)
axes[1].text(.08, .93,
             r"$u_I(t)=k_I\displaystyle\int_0^t e(\sigma)\,\mathrm{d}\sigma$",
             transform=axes[1].transAxes, fontsize=15, ha="left", va="top")
axes[1].text(.52, .15, r"$\displaystyle\int_0^t e(\sigma)\,\mathrm{d}\sigma$",
             transform=axes[1].transAxes, fontsize=14, color=ORANGE,
             ha="center", va="center")

figure.subplots_adjust(left=.06, right=.99, bottom=.04, top=.98, wspace=.18)
figure.savefig("ch07-pi-control-intuition.pdf", bbox_inches="tight", pad_inches=.03)
plt.close(figure)
