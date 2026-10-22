# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

"""Bounded-actuator teaching examples; not a physical reactor simulation.

Projected RK4 enforces rod stops. A step-halving check covers the full
sampled responses, including contact with the upper stop. The reset is
plotted separately from both continuous solution segments.
"""
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np

alpha, beta, kp, d = 0.4, 1.0, 2.0, 0.1
umax, tau_u, udot_max = 1.0, 0.5, 1.0
t_scram, q_scram = 4.0, 2.6
x_star = d / (beta * kp - alpha)
u_star = kp * x_star
x_crit = (beta * umax - d) / alpha
BLUE, ORANGE, GREEN = "#006CD4", "#d35000", "#27804c"


def actuator_rhs(x, u):
    speed = max(-udot_max, min(udot_max, (kp * x - u) / tau_u))
    if (u <= 0 and speed < 0) or (u >= umax and speed > 0):
        return 0.0
    return speed


def rhs(x, u):
    # Project stages as well as completed steps onto the physical interval.
    u = max(0.0, min(umax, u))
    return alpha * x + d - beta * u, actuator_rhs(x, u)


def integrate(initial, start, end, step):
    count = round((end - start) / step)
    stride = round(0.01 / step)
    assert abs(count * step - (end - start)) < 1e-10
    assert count % stride == 0
    x, u = initial
    samples = [(x, u)]
    crossing = None
    for i in range(count):
        a, b = rhs(x, u)
        c, e = rhs(x + step * a / 2, u + step * b / 2)
        f, g = rhs(x + step * c / 2, u + step * e / 2)
        h, j = rhs(x + step * f, u + step * g)
        next_x = x + step * (a + 2 * c + 2 * f + h) / 6
        u = max(0.0, min(umax, u + step * (b + 2 * e + 2 * g + j) / 6))
        if crossing is None and x < x_crit <= next_x:
            crossing = start + step * (i + (x_crit - x) / (next_x - x))
        x = next_x
        if (i + 1) % stride == 0:
            samples.append((x, u))
    states = np.asarray(samples).T
    assert np.isfinite(states).all()
    assert np.all((states[1] >= 0) & (states[1] <= umax))
    return np.linspace(start, end, len(samples)), states, crossing


def experiments(step):
    recovering = integrate((0.3, 0.0), 0, 12, step)
    slow = integrate((2.0, 0.0), 0, 12, step)
    pre = recovering[1][:, 400]
    jump = integrate((pre[0] + q_scram, pre[1]), 4, 12, step)
    return recovering, slow, jump


# Check full traces at common sample times, not only final values.
coarse = experiments(0.0002)
recovering, slow, jump = fine = experiments(0.0001)
for first, second in zip(coarse, fine):
    np.testing.assert_allclose(first[1], second[1], rtol=0, atol=3e-4)
    assert np.max(np.abs(np.diff(second[1][1]) / 0.01)) <= udot_max + 1e-8
assert abs(recovering[1][0, -1] - x_star) < 3e-5
assert abs(slow[2] - 0.3145) < 0.0001
assert jump[1][0, 0] > x_crit
np.testing.assert_allclose(jump[1][1, 0], recovering[1][1, 400])

# Exact scalar response from x(0)=2: full insertion until x=umax/kp,
# followed by the unsaturated affine equation. This comparison has no lag.
time = recovering[0]
x_switch = umax / kp
t_switch = np.log((x_switch - x_crit) / (2 - x_crit)) / alpha
instant_x = np.where(time <= t_switch,
                     x_crit + (2 - x_crit) * np.exp(alpha * time),
                     x_star + (x_switch - x_star) *
                     np.exp((alpha - beta * kp) * (time - t_switch)))
instant_u = np.clip(kp * instant_x, 0, umax)

plt.rcParams.update({"text.usetex": True, "font.family": "serif",
                     "font.serif": ["Computer Modern Roman"],
                     "font.size": 11, "axes.titlesize": 12,
                     "legend.fontsize": 10})


def phase_field(ax, xmin, xmax, upper):
    X, U = np.meshgrid(np.linspace(xmin, xmax, 27), np.linspace(0, upper, 21))
    DX = alpha * X + d - beta * U
    DU = np.vectorize(actuator_rhs)(X, U)
    ax.streamplot(X, U, DX, DU, density=0.65, color="0.84",
                  linewidth=0.6, arrowsize=0.7)
    ax.set(xlim=(xmin, xmax), ylim=(-0.025, upper + 0.045),
           xlabel=r"power deviation $x$", ylabel=r"rod insertion $u$")


def canvas():
    fig, axes = plt.subplots(2, 2, figsize=(10, 6.3),
                             gridspec_kw={"width_ratios": [1, 1.2]})
    phase, power, zoom, rods = axes.flat
    phase_field(phase, -0.08, 4.5, 1)
    phase.set_title("Phase paths; gray arrows: finite actuator", fontsize=11)
    phase.axvline(x_crit, ls="--", color="0.4", lw=1)
    phase.text(x_crit - 0.08, 0.65, r"$x_{\mathrm{crit}}=2.25$",
               fontsize=10, rotation=90, ha="right", va="center")
    phase.axhline(umax, color="0.4", ls=":", lw=1)
    phase.text(0.03, 0.89, r"$u_{\max}=1$", transform=phase.transAxes)
    phase_field(zoom, -0.05, 0.38, 0.50)
    zoom.set_title("Zoom near the equilibrium")
    zoom.plot(recovering[1][0], recovering[1][1], color=BLUE, lw=2)
    zoom.plot(x_star, u_star, "ko", ms=4)
    zoom.annotate(r"$(0.0625,0.125)$", (x_star, u_star),
                  xytext=(0.14, 0.08), fontsize=10,
                  arrowprops={"arrowstyle": "-", "lw": 0.7})
    zoom.plot(0.3, 0, "o", color=BLUE, ms=4)
    zoom.annotate(r"$(0.3,0)$", (0.3, 0), xytext=(0.25, 0.06), fontsize=10)
    for ax, label in [(power, r"power deviation $x$"), (rods, r"rod insertion $u$")]:
        ax.set(xlim=(0, 12), xlabel=r"normalized time $t$", ylabel=label)
        ax.grid(color="0.9", lw=0.6)
    power.set_title("Complete power response")
    power.axhline(x_crit, color="0.4", ls="--", lw=1)
    power.text(0.02, 0.14, r"$x_{\mathrm{crit}}=2.25$", transform=power.transAxes, fontsize=10)
    rods.set_title("Bounded rod insertion")
    rods.set_ylim(-0.04, 1.12)
    rods.axhline(umax, color="0.4", ls=":", lw=1)
    rods.text(0.98, 0.94, r"$u_{\max}=1$", ha="right", transform=rods.transAxes)
    return fig, phase, power, zoom, rods


def plot_case(phase, power, rods, case, color):
    t, states, _ = case
    phase.plot(*states, color=color, lw=2)
    power.plot(t, states[0], color=color, lw=2)
    rods.plot(t, states[1], color=color, lw=2)


def save(fig, name, handles, labels):
    fig.legend(handles, labels, loc="upper center", ncol=len(labels),
               frameon=False, bbox_to_anchor=(0.5, 1.005))
    fig.tight_layout(rect=(0, 0, 1, 0.95), h_pad=1.5, w_pad=2)
    output = Path(__file__).resolve().parent / name
    fig.savefig(output, bbox_inches="tight")
    plt.close(fig)
    print(f"Saved {output}")


fig, phase, power, zoom, rods = canvas()
plot_case(phase, power, rods, recovering, BLUE)
plot_case(phase, power, rods, slow, ORANGE)
phase.plot(instant_x, instant_u, color=GREEN, ls="--", lw=1.7)
power.plot(time, instant_x, color=GREEN, ls="--", lw=1.7)
rods.plot(time, instant_u, color=GREEN, ls="--", lw=1.7)
phase.plot(2, 0, "o", color=ORANGE, ms=4)
phase.annotate(r"$(2,0)$", (2, 0), xytext=(1.4, 0.14), fontsize=10,
               arrowprops={"arrowstyle": "-", "lw": 0.7})
phase.annotate("continues", (4.4, 1), xytext=(3.25, 0.76), fontsize=10,
               color=ORANGE, arrowprops={"arrowstyle": "->", "color": ORANGE})
rods.annotate(r"$\dot u=1$", (0.5, 0.5), xytext=(1.5, 0.6),
              arrowprops={"arrowstyle": "->", "color": ORANGE}, color=ORANGE)
power.set_ylim(-0.6, 27)
save(fig, "ch08-respect-unstable-toy.pdf",
     [plt.Line2D([], [], color=c, lw=2, ls=ls) for c, ls in
      [(BLUE, "-"), (ORANGE, "-"), (GREEN, "--")]],
     [r"finite speed: $(0.3,0)$", r"finite speed: $(2,0)$", r"instantaneous: $x(0)=2$"])

fig, phase, power, zoom, rods = canvas()
plot_case(phase, power, rods, recovering, BLUE)
# Crucially, the orange continuous path starts at jump[1][:, 0].
plot_case(phase, power, rods, jump, ORANGE)
pre, post = recovering[1][:, 400], jump[1][:, 0]
phase.annotate("", post, xytext=pre,
               arrowprops={"arrowstyle": "->", "ls": "--", "color": ORANGE})
phase.text(0.65, 0.19, r"jump $q_s=2.6$", color=ORANGE, fontsize=10)
phase.plot(*post, "o", color=ORANGE, ms=4)
phase.annotate("post-jump", post, xytext=(2.95, 0.05), fontsize=10)
phase.annotate("continues", (4.4, 1), xytext=(3.25, 0.76), fontsize=10,
               color=ORANGE, arrowprops={"arrowstyle": "->", "color": ORANGE})
power.plot([4, 4], [pre[0], post[0]], color=ORANGE, ls="--", lw=1.5)
power.plot(4, post[0], "o", color=ORANGE, ms=4)
power.annotate(r"jump at $t_s=4$", (4, post[0]), xytext=(0.5, 9),
               fontsize=10, arrowprops={"arrowstyle": "->", "lw": 0.7})
for ax in (power, rods):
    ax.axvline(4, color="0.55", ls=":", lw=0.8)
rods.text(4.12, 0.42, r"$t_s=4$", fontsize=10)
power.set_ylim(-0.5, 23)
save(fig, "ch08-respect-unstable-jump.pdf",
     [plt.Line2D([], [], color=c, lw=2) for c in (BLUE, ORANGE)],
     [r"no jump: $(0.3,0)$", r"after jump at $t_s=4$"])
print(f"Pre-jump state: {pre}; sufficient jump > {x_crit - pre[0]:.8f}")
print(f"Finite-speed crossing time: {slow[2]:.8f}")
print(f"Final powers: recovering={recovering[1][0,-1]:.8f}, "
      f"slow={slow[1][0,-1]:.8f}, jump={jump[1][0,-1]:.8f}")
