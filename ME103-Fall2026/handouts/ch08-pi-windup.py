# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp

plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"],
})

blues = ["#002447", "#0055A4", "#006CD4"]
oranges = ["#752d00", "#d35000", "#ff6100"]
grays = ["#333333", "#666666", "#cccccc", "#e6e6e6"]

tau = 1.0
kp = 2.0
ki = 1.0
umin, umax = -1.0, 1.0

def reference(t):
    return 2.0 if t < 6.0 else 0.5

def rhs(t, state, anti_windup):
    x, z = state
    error = reference(t) - x
    u_cmd = kp * error + ki * z
    u = np.clip(u_cmd, umin, umax)
    pushing_into_saturation = ((u_cmd >= umax and error > 0)
                               or (u_cmd <= umin and error < 0))
    dz = 0.0 if anti_windup and pushing_into_saturation else error
    return [(-x + u) / tau, dz]

time = np.linspace(0.0, 12.0, 2401)
solutions = {}
for label, anti_windup in [("ordinary integration", False),
                           ("freeze-on-saturation", True)]:
    sol = solve_ivp(rhs, (time[0], time[-1]), [0.0, 0.0],
                    args=(anti_windup,), t_eval=time, rtol=1e-8, atol=1e-10)
    x, z = sol.y
    r = np.array([reference(t) for t in time])
    error = r - x
    u_cmd = kp * error + ki * z
    u = np.clip(u_cmd, umin, umax)
    solutions[label] = (x, z, u_cmd, u)

fig, axes = plt.subplots(2, 2, figsize=(8.4, 5.4), sharex=True)
for col, (label, color) in enumerate(zip(solutions, [blues[1], oranges[1]])):
    x, z, u_cmd, u = solutions[label]
    axes[0, col].plot(time, x, color=color, lw=2.0, label=r"$x(t)$")
    axes[0, col].plot(time, [reference(t) for t in time],
                     color=grays[1], ls=":", lw=1.2, label=r"$r(t)$")
    axes[0, col].set_title(label)
    axes[0, col].set_ylabel(r"output $x$")
    axes[0, col].set_ylim(-0.2, 2.3)
    axes[0, col].grid(True, color=grays[3], lw=0.6)
    axes[0, col].legend(fontsize=9, loc="best")
    axes[1, col].plot(time, u_cmd, color=color, lw=1.8, label=r"$u_{\rm cmd}$")
    axes[1, col].plot(time, u, color=grays[0], lw=2.0, label=r"$u$")
    axes[1, col].plot(time, z, color=grays[2], ls="--", lw=1.4, label=r"$z$")
    axes[1, col].axhline(umax, color=grays[2], ls=":", lw=1.0)
    axes[1, col].axhline(umin, color=grays[2], ls=":", lw=1.0)
    axes[1, col].set_xlabel(r"time $t$")
    axes[1, col].set_ylabel(r"control / integral state")
    axes[1, col].set_ylim(-2.5, 5.0)
    axes[1, col].grid(True, color=grays[3], lw=0.6)
    axes[1, col].legend(fontsize=9, loc="best")

fig.suptitle(r"PI control with actuator saturation: $u=\mathrm{sat}(u_{\rm cmd})$")
fig.tight_layout(rect=[0, 0, 1, 0.95])
fig.savefig("ch08-pi-windup.pdf", bbox_inches="tight")
print("Saved ch08-pi-windup.pdf")
