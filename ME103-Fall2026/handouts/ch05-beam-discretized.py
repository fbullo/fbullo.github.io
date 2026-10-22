# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1.inset_locator import inset_axes, mark_inset

plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"]
})

# Color palette
blues = ['#002447', '#0055A4', '#006CD4', '#58b1ff']

# Parameters
n = 50
j = 1.0
k = 1.0

# Function to compute poles for given damping b
def compute_poles(b, color, label, ax, in_ax):
    diag_K = np.full(n, 2.0 * k); diag_K[-1] = k
    off_K  = np.full(n - 1, -k)
    K_tri  = np.diag(diag_K) + np.diag(off_K, 1) + np.diag(off_K, -1)

    diag_D = np.full(n, 2.0 * b); diag_D[-1] = b
    off_D  = np.full(n - 1, -b)
    D_tri  = np.diag(diag_D) + np.diag(off_D, 1) + np.diag(off_D, -1)

    I_n = np.eye(n)
    Z_n = np.zeros((n, n))
    M_inv = (1.0 / j) * I_n

    A_top = np.hstack((Z_n, I_n))
    A_bot = np.hstack((-M_inv @ K_tri, -M_inv @ D_tri))
    A = np.vstack((A_top, A_bot))

    evals = np.linalg.eigvals(A)

    ax.scatter(evals.real, evals.imag, marker='x', color=color,
               s=9, linewidths=0.8, label=label)  # 6 * 1.5 = 9
    in_ax.scatter(evals.real, evals.imag, marker='x', color=color,
                  s=18, linewidths=0.8)  # 12 * 1.5 = 18

# Plot on complex plane
fig, ax = plt.subplots(figsize=(6, 4))
in_ax = inset_axes(ax, width="30%", height="30%", loc="upper center", borderpad=0.4)

# Damping values and colors
b_values = [0.1, 1, 2, 5]
for i, b in enumerate(b_values):
    compute_poles(b=b, color=blues[i], label=fr"$b={b}$", ax=ax, in_ax=in_ax)

# Main plot settings
ax.set_title(r'Poles of $n=50$ segment beam model')
ax.set_xlabel(r"Re$(\lambda)$")
ax.set_ylabel(r"Im$(\lambda)$")
ax.grid(True)
ax.legend()

# Inset settings
in_ax.set_xlim(-0.015, 0.015)
in_ax.set_ylim(-0.05, 0.05)
in_ax.set_xticks([in_ax.get_xlim()[0], in_ax.get_xlim()[1]])
in_ax.set_yticks([in_ax.get_ylim()[0], in_ax.get_ylim()[1]])
in_ax.axhline(0, linewidth=0.8)
in_ax.axvline(0, linewidth=0.8)
mark_inset(ax, in_ax, loc1=1, loc2=3, fc="none", ec="#999999", lw=0.3)

# Save to PDF
fig.savefig("ch05-beam-discretized.pdf", bbox_inches='tight')



