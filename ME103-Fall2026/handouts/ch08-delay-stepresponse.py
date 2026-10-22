# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Define the requested colors from darker to lighter shades
blues = ['#002447', '#003c76', '#0055A4', '#006CD4', '#0085ff', '#239cff', '#58b1ff']
oranges = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']

# Parameters
tau = 0.3
T = 1.0
k_values = [0.5, 1.0, 1.2, 1.5, 2.0]   # try larger k to see instability emerge

# k_crit ≈ 1.2497 for tau=0.3, T=1.0: the first crossing solves
# omega*T + atan(tau*omega) = pi, omega ≈ 2.498, k_crit = sqrt(1+(tau*omega)^2)
K_CRIT = 1.2497

# Discretization
h = 0.0001                     # time step (choose h << min(tau, T))
t_final = 14.0
N = int(np.round(t_final / h))
m = int(np.round(T / h))        # delay in samples

# Reference: unit step
r = np.ones(N + 1)
t = np.linspace(0.0, t_final, N + 1)

plt.figure(figsize=(7, 3))

for i, k in enumerate(k_values):
    # Pre-allocate with m+N+1 to accommodate negative-time history (zeros)
    y = np.zeros(m + N + 1)

    # Time loop (method of steps, forward Euler)
    for n in range(N):
        yn    = y[m + n]        # y(t_n)
        y_del = y[n]            # y(t_n - T)
        r_del = r[n - m] if n >= m else 0.0   # r(t_n - T) = 1(t_n - T): zero until t = T (fixed 2026-09-24)
        # Euler update:
        y[m + n + 1] = yn + (h / tau) * ( -yn + k * (r_del - y_del) )

        # Choose color: use blue shades if k < K_CRIT, orange shades if k > K_CRIT
        if k < K_CRIT:
            color = blues[(i + 2) % len(blues)]
        else:
            color = oranges[(i + 2) % len(oranges)]
            # Choose color: default blues[i], but use oranges[4] when k=1.5
        
    plt.plot(t, y[m : m + N + 1], label=fr"$k={k}$", color=color)

plt.xlabel(r"time $t$")
plt.ylabel(r"response $y(t)$")
plt.title(r"Step response of $\tau\dot y + y = k(\mathbf{1}(t-T) - y(t-T))$")
plt.grid(True, ls=":")
plt.ylim(-2.5, 2.5)
plt.xlim(0, t_final)
plt.legend()
plt.tight_layout()
plt.savefig("ch08-delay-stepresponse.pdf", bbox_inches="tight")
plt.close()


