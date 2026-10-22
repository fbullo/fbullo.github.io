# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import matplotlib.pyplot as plt
import numpy as np

plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"],
})

# Define pole locations
pole_real = -3
pole_imag = np.sqrt(2)

def create_complex_plane_axes():
    """Create and configure the complex-plane axes."""
    fig, ax = plt.subplots(figsize=(2, 2))

    ax.axhline(0, color="black", lw=1)
    ax.axvline(0, color="black", lw=1)
    ax.set_xlim(-4, 1)
    ax.set_ylim(-2.5, 2.5)
    ax.set_aspect("equal")
    ax.tick_params(axis="both", labelsize=9.3)

    # Show the pole coordinates to three significant digits.
    imag_label = f"{pole_imag:.3g}"
    ax.set_xticks([pole_real])
    ax.set_yticks([pole_imag, -pole_imag])
    ax.set_xticklabels([r"$-3$"])
    ax.set_yticklabels([
        rf"$\mathrm{{i}}\,{imag_label}$",
        rf"$-\mathrm{{i}}\,{imag_label}$",
    ])

    # Arrowheads on the positive real and imaginary axes.
    ax.plot(1, 0, ">k", transform=ax.get_yaxis_transform(), clip_on=False)
    ax.plot(0, 1, "^k", transform=ax.get_xaxis_transform(), clip_on=False)

    ax.tick_params(axis="x", which="both", length=0)
    ax.tick_params(axis="y", which="both", length=0)
    for spine in ax.spines.values():
        spine.set_visible(False)

    return fig, ax


def draw_poles(ax):
    """Draw the conjugate poles and their coordinate guides."""
    ax.plot(
        pole_real,
        pole_imag,
        "x",
        color="#0055A4",
        markersize=10,
        markeredgewidth=3,
    )
    ax.plot(
        pole_real,
        -pole_imag,
        "x",
        color="#0055A4",
        markersize=10,
        markeredgewidth=3,
    )
    ax.axvline(pole_real, color="black", linestyle="--", lw=0.5)
    ax.axhline(pole_imag, color="black", linestyle="--", lw=0.5)
    ax.axhline(-pole_imag, color="black", linestyle="--", lw=0.5)


fig, ax = create_complex_plane_axes()
draw_poles(ax)
plt.savefig("ch05-ex-frompolestofunction.pdf", bbox_inches="tight")
plt.close(fig)
