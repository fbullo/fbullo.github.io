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
    "text.latex.preamble": r"\usepackage{amsmath,amssymb,amsfonts}"
})



def plot_rlocus(sys, filename, title=None, xlim=None, ylim=None):
    fig, ax = plt.subplots(figsize=(4.0, 3.2), constrained_layout=True)
    ctrl.root_locus(sys, plot=True, grid=False, title="")

    # Axes lines
    ax.axhline(0, lw=0.6, color="black")
    ax.axvline(0, lw=0.6, color="black")

    # Labels / title
    ax.set_xlabel("Re$(s)$")
    ax.set_ylabel("Im$(s)$")
    if title:
        ax.set_title(title)

    # Limits
    if xlim: ax.set_xlim(xlim)
    if ylim: ax.set_ylim(ylim)

    fig.savefig(f"{filename}.pdf")
    plt.close(fig)

# ----------------------------
# UNCOMPENSATED: G(s) = 1/(s(s+2))
# ----------------------------
G_uncomp = ctrl.tf([1], np.polymul([1, 0], [1, 2]))  # denominator s*(s+2)
plot_rlocus(
    G_uncomp,
    filename="ch08-rootlocus-ctrl-uncompensated",
    title=r"$G_{\text{P control}}(s)=\dfrac{1}{s(s+2)}$",
    xlim=(-22, 2),
    ylim=(-8, 8),
)

# ----------------------------
# DERIVATIVE ACTION: C(s) = s+4  ->  L0(s) = (s+4)G(s)
# ----------------------------
C_derivative = ctrl.tf([1, 4], [1])
L0_derivative = ctrl.series(C_derivative, G_uncomp)
plot_rlocus(
    L0_derivative,
    filename="ch08-rootlocus-ctrl-derivative",
    title=r"$G_{\text{PD control}}(s)=(s+4)\,G(s)$",
    xlim=(-22, 2),
    ylim=(-8, 8),
)

# ----------------------------
# PROPER LEAD: C(s) = (s+4)/(s+20)  ->  L0(s) = C(s)G(s)
# ----------------------------
C_lead = ctrl.tf([1, 4], [1, 20])
L0_lead = ctrl.series(C_lead, G_uncomp)
plot_rlocus(
    L0_lead,
    filename="ch08-rootlocus-ctrl-lead",
    title=r"$G_{\text{lead compensator}}(s)=\frac{(s+4)}{s+20}\,G(s)$",
    xlim=(-22, 2),
    ylim=(-8, 8),
)

