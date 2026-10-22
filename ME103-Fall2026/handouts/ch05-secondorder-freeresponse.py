# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import matplotlib.pyplot as plt; import numpy as np; import control as ctrl

# System parameters
omegan = 1.0  # Natural frequency
zeta_values = [0.0, 0.05, 0.1, 0.2, 0.4, 0.5, 0.8, 1.0, 1.1, 1.5, 2.0]  # Damping ratios
bg_color = '#f5f5f5'  # Background color

# Time domain for simulation
t = np.linspace(0, 12, 1000)

# Loop through each zeta value
for zeta in zeta_values:
    # Compute and plot the free response from x(0)=1 and zero initial velocity.
    # Use an explicit state-space model with state (x, xdot) and output x, so
    # that X0 is unambiguous (2026-09-24: initial_response on the transfer
    # function used python-control's internal state order, and X0=[0,-1]
    # started every curve at x(0)=-1).
    A = [[0, 1], [-omegan**2, -2 * zeta * omegan]]
    ss_model = ctrl.ss(A, [[0], [omegan**2]], [[1, 0]], [[0]])
    T, yout = ctrl.initial_response(ss_model, T=t, X0=[1, 0])
    plt.figure(figsize=(6, 2))
    plt.plot(T, yout, color='#0055A4')
    plt.ylim(-1.1, 1.1); plt.xlim(0, 12)
    # Add plots of the envelope +-e^(-zeta*t)/sqrt(1-zeta^2) for zeta < 1
    if zeta < 1:
        exponential_decay = np.exp(-zeta * t) / np.sqrt(1-zeta**2)
        plt.plot(T, exponential_decay, '--', color='#666666', lw=1)
        plt.plot(T, -exponential_decay, '--', color='#666666', lw=1)
    plt.grid(True); plt.gca().set_facecolor(bg_color);
    ax=plt.gca(); ax.set_xticklabels([])
    tag = f"{zeta:.2f}".replace(".", "p")
    plt.savefig(f"ch05-secondorder-freeresponse-zeta-{tag}.pdf", bbox_inches='tight')
    plt.close()

    # Compute and plot the free response from x(0)=0 and unit initial velocity
    # (used in the table of the four classes; for omegan=1, this is also the
    # impulse response of the transfer function omegan^2/(s^2+2 zeta omegan s+omegan^2))
    T, yout = ctrl.initial_response(ss_model, T=t, X0=[0, 1])
    plt.figure(figsize=(6, 2))
    plt.plot(T, yout, color='#0055A4')
    plt.ylim(-1.1, 1.1); plt.xlim(0, 12)
    # Add plots of the envelope +-e^(-zeta*omegan*t)/omegad for zeta < 1
    if zeta < 1:
        exponential_decay = np.exp(-zeta * omegan * t) / (omegan * np.sqrt(1-zeta**2))
        plt.plot(T, exponential_decay, '--', color='#666666', lw=1)
        plt.plot(T, -exponential_decay, '--', color='#666666', lw=1)
    plt.grid(True); plt.gca().set_facecolor(bg_color);
    ax=plt.gca(); ax.set_xticklabels([])
    plt.savefig(f"ch05-secondorder-freeresponse-velocity-zeta-{tag}.pdf", bbox_inches='tight')
    plt.close()
