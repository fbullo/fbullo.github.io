# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

# Block diagram for Chapter 6: a sinusoidal input u(t)=sin(omega t) forcing an
# undamped second-order system, whose output is a sum of two sinusoids that
# beat. Replaces the OmniGraffle figure Figures/bd-freqResponse-beatings.pdf,
# whose labels still used the old symbol omega_u (2026-09-24).
import os
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"],
    "text.latex.preamble": r"\usepackage{amsmath,amssymb,amsfonts}",
})

blues = ['#002447', '#003c76', '#0055A4', '#006CD4', '#0085ff', '#239cff', '#58b1ff']
oranges = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']
grays = ['#1a1a1a', '#333333', '#4d4d4d', '#666666', '#999999', '#cccccc', '#e6e6e6']
BROWN = oranges[3]   # gnbrown4: labels revised 2026-09-24

fig, ax = plt.subplots(figsize=(10, 1.55))
ax.set_xlim(0, 10); ax.set_ylim(-0.12, 1.45); ax.axis('off')

y_arrow = 0.30          # height of the signal arrows
arrow = dict(arrowstyle='-|>', color=grays[4], lw=1.0, mutation_scale=12)

# Input sinusoid (upper left) and input arrow
x_in = np.linspace(0.0, 2.05, 600)
ax.plot(x_in, 0.95 + 0.42*np.sin(2*np.pi*4.5*x_in/2.05), color=oranges[2], lw=1.8)
ax.annotate('', xy=(2.30, y_arrow), xytext=(0.0, y_arrow), arrowprops=arrow)
ax.text(1.05, 0.14, r'$u(t)=\sin(\omega t)$', ha='center', va='top',
        fontsize=13, color=BROWN)

# Transfer function block
box = FancyBboxPatch((2.32, 0.00), 2.00, 0.62, boxstyle='square,pad=0',
                     facecolor=grays[5], edgecolor='black', lw=1.0)
ax.add_patch(box)
ax.text(3.32, 0.31, r'$G(s)=\dfrac{\omega_{\mathrm{n}}^2}{s^2+\omega_{\mathrm{n}}^2}$',
        ha='center', va='center', fontsize=14)

# Output arrow and beating output signal (upper right)
ax.annotate('', xy=(10.0, y_arrow), xytext=(4.32, y_arrow), arrowprops=arrow)
x_out = np.linspace(4.50, 9.95, 6000)
tau = (x_out - x_out[0]) / (x_out[-1] - x_out[0])
envelope = np.cos(2*np.pi*1.2*tau)          # a little more than two beats
carrier = np.sin(2*np.pi*140*tau)
ax.plot(x_out, 0.95 + 0.45*envelope*carrier, color=blues[5], lw=0.8)
ax.text(7.25, 0.14, r'sum of two sinusoids at frequencies $\omega\approx\omega_{\mathrm{n}}$',
        ha='center', va='top', fontsize=13, color=BROWN)

outdir = os.path.dirname(os.path.abspath(__file__))
fig.savefig(os.path.join(outdir, 'ch06-freqresponse-beatings-diagram.pdf'),
            bbox_inches='tight')
plt.close(fig)
