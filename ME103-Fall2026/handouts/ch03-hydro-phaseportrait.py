# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

# Python libraries
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch

plt.rcParams.update({"text.usetex": True, "font.family": "serif",
                     "font.serif": ["Computer Modern Roman"], "font.size": 9})

# One-dimensional phase portraits, in the same style throughout the book.
# The same function is in ch01-phase1d.py, ch01-sin-phaseportrait.py,
# ch03-watertank-phaseportrait.py, ch03-hydro-phaseportrait.py and
# ch08-saturation-instability-1.py: keep the five copies identical.

BLUE = "#006CD4"   # graph of f and flow arrows
GRAY = "#999999"   # dashed lines from the zeros of f down to the phase line


def phase_portrait(f, xmin, xmax, fmin, fmax, equilibria, labels,
                   xname="x", fname="f(x)", width=3.46, start=None):
    """Graph of f (above) and phase line of dx/dt = f(x) (below).

    xmin, xmax  range of x
    fmin, fmax  range of f(x) shown in the graph
    equilibria  zeros of f, marked by dots; labels are their names
    width       width of the figure in inches; all other sizes are fixed
                in inches, so that all phase portraits look alike
    start       left end of the state space, when it is a half-line
    """
    graph, top, gap, bottom = 1.0, 0.18, 0.25, 0.2     # heights (inches)
    left, right = 0.15, 0.27                           # margins (inches)
    sx = (width - left - right) / (xmax - xmin)        # inches per unit of x
    sy = graph / (fmax - fmin)                         # inches per unit of f
    yline = fmin - gap / sy                            # height of phase line
    fig = plt.figure(figsize=(width, top + graph + gap + bottom))
    ax = fig.add_axes([0, 0, 1, 1])
    ax.set_xlim(xmin - left / sx, xmax + right / sx)
    ax.set_ylim(yline - bottom / sy, fmax + top / sy)
    ax.axis("off")

    # axes: x for the graph and for the phase line, f(x) for the graph
    x0 = xmin if start is None else start
    xv = 0 if xmin <= 0 <= xmax else x0
    tip = xmax + 0.12 / sx
    axis = dict(arrowstyle="-|>", lw=0.6, color="black", mutation_scale=7,
                shrinkA=0, shrinkB=0)
    for y in (0, yline):
        ax.annotate("", xy=(tip, y), xytext=(x0, y), arrowprops=axis)
        ax.text(tip + 0.04 / sx, y, "$%s$" % xname, va="center")
    ax.annotate("", xy=(xv, fmax), xytext=(xv, fmin), arrowprops=axis)
    ax.text(xv, fmax + 0.03 / sy, "$%s$" % fname, ha="center", va="bottom")

    # graph of f, cut where it leaves the range from fmin to fmax
    x = np.linspace(x0, xmax, 1201)
    with np.errstate(all="ignore"):
        y = f(x)
    y[(y < fmin) | (y > fmax)] = np.nan
    ax.plot(x, y, color=BLUE, lw=1.5)

    # equilibria: dots on the graph and on the phase line, a dashed line between
    for xe, label in zip(equilibria, labels):
        ax.plot([xe, xe], [0, yline], color=GRAY, lw=0.6, ls=(0, (3, 2)))
        ax.plot([xe, xe], [0, yline], "o", ms=4.5, color="black", zorder=5)
        ax.text(xe, yline - 0.07 / sy, "$%s$" % label, ha="center", va="top")
    if start is not None and start not in equilibria:
        ax.plot([start, start], [yline - 0.035 / sy, yline + 0.035 / sy],
                color="black", lw=0.6)
        ax.text(start, yline - 0.07 / sy, "$%g$" % start, ha="center", va="top")

    # flow arrows: in each interval, as many as fit, in the direction of f
    length, space, margin = 0.17, 0.08, 0.07           # inches
    ends = [x0] + sorted(e for e in equilibria if x0 < e < xmax) + [xmax]
    for a, b in zip(ends[:-1], ends[1:]):
        a1 = a + (margin if a in equilibria or a == start else 0.03) / sx
        b1 = b - (margin if b in equilibria else 0.03) / sx
        room = (b1 - a1) * sx                          # inches available
        if room < 0.1:
            continue
        n = max(1, int((room + space) // (length + space)))
        L = min(length, room)
        s = a1 + (room - n * L - (n - 1) * space) / 2 / sx
        sign = np.sign(f((a + b) / 2))
        for k in range(n):
            p = s + k * (L + space) / sx
            q = p + L / sx
            if sign < 0:
                p, q = q, p
            ax.add_patch(FancyArrowPatch(
                (p, yline), (q, yline), color=BLUE, lw=0, zorder=4,
                arrowstyle="simple,head_length=0.55,head_width=0.6,tail_width=0.2",
                mutation_scale=10, shrinkA=0, shrinkB=0))
    return fig

# Hydroelectric dam: d(omega)/dt = A/omega - B, where
# A = c_t sqrt(rho) (g h)^(3/2) / (I r) and B = tau_g / I; here A = B = 1
A, B = 1, 1
omega_star = A / B

fig = phase_portrait(lambda w: A / w - B, 0, 2.6, -0.8, 1.2, [omega_star],
                     [r"\omega^*"], xname=r"\omega", fname=r"\dot{\omega}",
                     width=0.5 * 6.92, start=0)
fig.savefig("ch03-hydro-phaseportrait.pdf")
