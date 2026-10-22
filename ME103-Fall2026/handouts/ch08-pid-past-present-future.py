# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

# PID intuition: integral action uses the past error (hatched area), proportional
# action the present error, and derivative action the present slope (arrow), a
# hint of the near-future trend. Redraw of the hand sketch hand-past+present+future.jpg.
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import CubicHermiteSpline

plt.rcParams.update({"text.usetex": True, "font.family": "serif",
                     "font.serif": ["Computer Modern Roman"], "hatch.linewidth": 1.0})
blues = ['#002447', '#003c76', '#0055A4', '#006CD4', '#0085ff', '#239cff', '#58b1ff']
grays = ['#1a1a1a', '#333333', '#4d4d4d', '#666666', '#999999', '#cccccc', '#e6e6e6']

# Error signal through a few knots (time, value, slope); the last knot is the present
knots = np.array([[-2.1, -0.77, 0.80], [-1.2, 0.0, 0.70], [0.4, 0.68, 0.0], [2.0, 0.0, -0.62],
                  [4.2, -1.05, 0.0], [6.4, 0.0, 0.78], [8.75, 1.5, 0.35]])
tp, ep, slope = knots[-1]
e = CubicHermiteSpline(knots[:, 0], knots[:, 1], knots[:, 2])
t = np.linspace(knots[0, 0], tp, 400)

fig, ax = plt.subplots(figsize=(9, 4))
arrow = dict(arrowstyle='-|>', color=grays[2], lw=1.3, mutation_scale=18)
ax.annotate('', xy=(15.2, 0), xytext=(-2.5, 0), arrowprops=arrow)   # time axis
ax.annotate('', xy=(0, 5.4), xytext=(0, -1.8), arrowprops=arrow)    # error axis
ax.text(15.45, 0, '$t$', fontsize=26, va='center')
ax.text(0.45, 5.1, '$e(t)$', fontsize=26, va='center')

# past: the accumulated error (integral action)
ax.fill_between(t, e(t), 0, facecolor='none', edgecolor=blues[6], hatch='///', lw=0)
ax.plot(t, e(t), color=blues[3], lw=2.5)
# present: the current error (proportional action) and its slope (derivative action)
ax.plot([tp, tp], [-1.35, 4.2], '--', color=grays[2], lw=1.3)
ax.text(tp, 4.55, 'Present', fontsize=22, ha='center')
ax.text(tp, -1.5, '$t$', fontsize=26, ha='center', va='top')
ax.annotate('', xy=(tp + 3.0, ep + 3.0*slope), xytext=(tp, ep),
            arrowprops=dict(arrowstyle='-|>', color=blues[5], lw=2.2, mutation_scale=18))
ax.annotate('', xy=(3.6, 2.75), xytext=(8.0, 2.75), arrowprops=arrow)
ax.text(5.8, 3.0, 'Past', fontsize=22, ha='center')
ax.annotate('', xy=(14.3, 3.05), xytext=(9.7, 3.05), arrowprops=arrow)
ax.text(12.0, 3.3, 'Near future', fontsize=22, ha='center')

ax.set_xlim(-2.6, 16.2); ax.set_ylim(-2.3, 5.6); ax.set_aspect('equal'); ax.axis('off')
fig.savefig('ch08-pid-past-present-future.pdf', bbox_inches='tight')
