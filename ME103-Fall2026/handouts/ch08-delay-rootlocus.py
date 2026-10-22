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

# Fixed Padé orders
FIXED_ORDERS = [1, 2, 3, 4]

# Fixed axis limits
XLIM = (-12, 12)
YLIM = (-7, 7)

# Constants (set directly here instead of using arguments); the same values
# as the caption of this figure and as ch08-delay-stepresponse.py (2026-09-24)
tau = 0.3      # Time constant tau (>0)
T = 1.0        # Delay T (>0)

# gnbrown4: marks added 2026-09-24
BROWN = '#d35000'

def critical_gain(G, Kmax=50.0):
    """Smallest K>0 at which a root of den + K num reaches the imaginary axis."""
    num, den = G.num[0][0], G.den[0][0]
    top = lambda K: np.max(np.roots(np.polyadd(den, K*np.asarray(num))).real)
    lo, hi = 0.0, Kmax
    for _ in range(200):
        mid = 0.5*(lo + hi)
        lo, hi = (mid, hi) if top(mid) < 0 else (lo, mid)
    roots = np.roots(np.polyadd(den, hi*np.asarray(num)))
    return hi, roots[np.abs(roots.real) < 1e-6]

def make_open_loop(tau: float, T: float, order: int):
    """Build open-loop transfer function G0(s)*Padé(order)(s) (without gain K)."""
    G0 = ctrl.tf([1.0], [tau, 1.0])        # 1/(tau s + 1)
    numD, denD = ctrl.pade(T, order)       # Padé for e^{-sT}
    D = ctrl.tf(numD, denD)
    return G0 * D

def plot_and_save_root_locus(G: ctrl.TransferFunction, order: int):
    plt.figure(figsize=(7, 4))

    ctrl.root_locus(G, plot=True, grid=False, title="")
    
    ax = plt.gca()
    ax.axvline(0.0, color='k', lw=0.6)
    ax.axhline(0.0, color='k', lw=0.6)

    # Mark where the locus crosses the imaginary axis
    Kc, sc = critical_gain(G)
    ax.plot(sc.real, sc.imag, 'o', color=BROWN, ms=6, zorder=5)
    yc = np.max(np.abs(sc.imag))
    ax.annotate(rf'$K={Kc:.2f}$', xy=(0, yc), xytext=(8, 6), textcoords='offset points',
                color=BROWN, fontsize=12, ha='left', va='bottom')

    # Fixed axis limits
    plt.xlim(XLIM)
    plt.ylim(YLIM)

    plt.grid(True, which='both', ls=':')
    plt.xlabel(r'Re$(s)$')
    plt.ylabel(r'Im$(s)$')

    ordinals = {
        1: "first-order",
        2: "second-order",
        3: "third-order",
        4: "fourth-order",
    }
    title_text = ordinals.get(order, f"{order}th-order") + " Padé approximant"
    plt.title(rf'Root locus of $K\,\frac{{1}}{{\tau s+1}}\cdot$ {title_text} ($T={T}$, $\tau={tau}$)')

    plt.tight_layout()
    pdf_name = f'ch08-delay-rootlocus-pade{order}.pdf'
    plt.savefig(pdf_name)
    plt.close()
    print(f'Saved: {pdf_name}')

# for order in FIXED_ORDERS:
#     G_ol = make_open_loop(tau, T, order)
#     plot_and_save_root_locus(G_ol, order)
#    print(G_ol)

# The former "hack" flipped the numerator sign for odd orders; that plots
# the locus of 1 - K G(s) = 0, not of 1 + K G(s) = 0. python-control 0.10
# needs no workaround (checked 2026-09-24 against the crossing gains).
for order in FIXED_ORDERS:
    G_ol = make_open_loop(tau, T, order)
    plot_and_save_root_locus(G_ol, order)
    print(G_ol)
