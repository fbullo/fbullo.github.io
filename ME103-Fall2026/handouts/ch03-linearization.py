# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import odeint

plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"]
})

# Parameters
g = 9.81     # gravity
ell = 1.0    # pendulum length

# Grid for phase portrait
theta_range, omega_range = np.meshgrid(
    np.linspace(-3*np.pi/2, 3*np.pi/2, 20),
    np.linspace(-7, 7, 20)
)

# Exact dynamics
dtheta_exact = omega_range
domega_exact = -g/ell * np.sin(theta_range)
magnitude_exact = np.hypot(dtheta_exact, domega_exact)

# Linearized dynamics
dtheta_linear = omega_range
domega_linear = -g/ell * theta_range
magnitude_linear = np.hypot(dtheta_linear, domega_linear)

# Plot: Exact Dynamics Phase Portrait
plt.figure(figsize=(9, 6))
plt.streamplot(theta_range, omega_range, dtheta_exact, domega_exact,
               density=0.75, linewidth=magnitude_exact / 2,
               color='#0085ff', arrowsize=2.5)
plt.xlabel(r'$\theta$', fontsize=20)
plt.ylabel(r'$\omega$', fontsize=20)
plt.xticks(
    [-3*np.pi/2, -np.pi, -np.pi/2, 0, np.pi/2, np.pi, 3*np.pi/2],
    [r'$-3\pi/2$', r'$-\pi$', r'$-\pi/2$', r'$0$', r'$\pi/2$', r'$\pi$', r'$3\pi/2$'],
    fontsize=20
)
plt.yticks(fontsize=20)
plt.xlim([-3*np.pi/2, 3*np.pi/2])
plt.ylim([-7, 7])
plt.grid(True)
for scatter_theta in [-np.pi, 0, np.pi]:
    plt.scatter(scatter_theta, 0, color='black', s=50, zorder=5)
plt.tight_layout()
plt.savefig("ch03-linearization-sintheta.pdf", bbox_inches='tight')

# Plot: Linearized Dynamics Phase Portrait
plt.figure(figsize=(9, 6))
plt.streamplot(theta_range, omega_range, dtheta_linear, domega_linear,
               density=0.75, linewidth=magnitude_linear / 2,
               color='#a43e00', arrowsize=2.5)
plt.xlabel(r'$\theta$', fontsize=20)
plt.ylabel(r'$\omega$', fontsize=20)
plt.xticks(
    [-3*np.pi/2, -np.pi, -np.pi/2, 0, np.pi/2, np.pi, 3*np.pi/2],
    [r'$-3\pi/2$', r'$-\pi$', r'$-\pi/2$', r'$0$', r'$\pi/2$', r'$\pi$', r'$3\pi/2$'],
    fontsize=20
)
plt.yticks(fontsize=20)
plt.xlim([-3*np.pi/2, 3*np.pi/2])
plt.ylim([-7, 7])
plt.grid(True)
plt.scatter(0, 0, color='black', s=50, zorder=5)
plt.tight_layout()
plt.savefig("ch03-linearization-theta.pdf", bbox_inches='tight')


