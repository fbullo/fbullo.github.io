# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt

# --- LaTeX + AMS ---
plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"],
    "text.latex.preamble": r"\usepackage{amsmath,amssymb,amsfonts}"
})

def draw_complex_number():
    r = 1.0
    theta = np.deg2rad(60)
    R_circle = 1.0
    bluecol = "#58b1ff"
    col = "black"
    lw = 2.0

    # font sizes (50% larger than before)
    fs_axis = 30
    fs_z = 33
    fs_misc = 30
    fs_C = 42

    fig, ax = plt.subplots(figsize=(6.8, 6.0))
    ax.set_aspect("equal", adjustable="box")
    ax.set_xlim(-1.2*R_circle, 1.6*R_circle)
    ax.set_ylim(-1.2*R_circle, 1.3*R_circle)
    ax.axis("off")

    # Circle (fill + edge)
    ax.add_patch(plt.Circle((0, 0), R_circle, facecolor=col, alpha=0.12, edgecolor=col, lw=lw))
    ax.add_patch(plt.Circle((0, 0), R_circle, facecolor="none", edgecolor=col, lw=lw))

    # Axes with larger arrowheads
    arrow_kw = dict(arrowstyle="-|>", lw=lw, color=col, mutation_scale=28)
    ax.annotate("", xy=(1.6*R_circle, 0), xytext=(-1.2*R_circle, 0), arrowprops=arrow_kw)  # Re axis
    ax.annotate("", xy=(0, 1.2*R_circle), xytext=(0, -1.2*R_circle), arrowprops=arrow_kw)  # Im axis

    # Radius and point
    zx, zy = r*np.cos(theta), r*np.sin(theta)
    ax.plot([0, zx], [0, zy], color=col, lw=lw)
    ax.plot(zx, zy, "o", ms=14, mfc=bluecol, mec=bluecol)

    # Theta arc + arrow
    th = np.linspace(np.deg2rad(2), theta-np.deg2rad(2), 200)
    arc_r = 0.78*R_circle
    arc_x = arc_r*np.cos(th)
    arc_y = arc_r*np.sin(th)
    ax.plot(arc_x, arc_y, color=col, lw=lw)

    # Short arrow at the end of the arc
    t_tip = 0.98*theta  # where along the arc the arrow sits
    tip = np.array([arc_r*np.cos(t_tip), arc_r*np.sin(t_tip)])
    tdir = np.array([-np.sin(t_tip), np.cos(t_tip)])  # tangent direction
    tdir = tdir / np.linalg.norm(tdir)
    arc_arrow_half_len = 0.03  # shorter arrow length
    ax.annotate(
        "",
        xy=tip + arc_arrow_half_len*tdir,
        xytext=tip - arc_arrow_half_len*tdir,
        arrowprops=dict(arrowstyle="-|>", lw=lw, color=col, mutation_scale=24)
    )

    # Labels (bigger fonts)
    ax.text(1.6*R_circle, -0.25*R_circle, r"$\mathrm{Re}(z)$", fontsize=fs_axis, color=col, ha="right")
    # Move Im(z) to the LEFT of the vertical axis
    ax.text(-0.10*R_circle, 1.05*R_circle, r"$\mathrm{Im}(z)$", fontsize=fs_axis, color=col, va="bottom", ha="right")

    ax.text(zx+0.04*R_circle, zy+0.06*R_circle, r"$z$", fontsize=fs_z, color=col)
    ax.text(0.52*zx-0.03, 0.52*zy-0.2, r"$r$", fontsize=fs_misc, color=col)
    ax.text(arc_r*np.cos(0.55*theta)+0.03, arc_r*np.sin(0.55*theta)-0.02,
            r"$\theta$", fontsize=fs_misc, color=col)
    ax.text(1.25*R_circle, 1.0*R_circle, r"$\mathbb{C}$", fontsize=fs_C, color=col)

    fig.savefig("ch04-complex-number.pdf", bbox_inches="tight")
    plt.close(fig)

# Save (no display)
draw_complex_number()
