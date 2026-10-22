# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import matplotlib.pyplot as plt
import numpy as np
import control as ctrl


plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"],
})


# G(s) = 10(s + 1) / [s(s + 10)]
system = ctrl.TransferFunction([10, 10], [1, 10, 0])

time = np.linspace(0, 5, 800)
time, step_response = ctrl.step_response(system, T=time)

frequency = np.logspace(-2, 2, 800)
magnitude, phase, _ = ctrl.freqresp(system, frequency)
magnitude = np.squeeze(np.abs(magnitude))
phase = np.squeeze(phase) * 180 / np.pi


fig, (step_ax, magnitude_ax, phase_ax) = plt.subplots(
    3, 1, figsize=(7.5, 7.2), sharex=False
)

step_ax.plot(time, step_response, color="#0055A4", linewidth=1.8)
step_ax.set_title(r"Step response", fontsize=15, pad=6)
step_ax.set_xlabel(r"Time $t$ (s)", fontsize=12)
step_ax.set_ylabel(r"$y(t)$", fontsize=12)
step_ax.grid(True, alpha=0.35)

magnitude_ax.semilogx(
    frequency, 20 * np.log10(magnitude), color="#0055A4", linewidth=1.8
)
magnitude_ax.axvline(1, color="#a43e00", linestyle="--", linewidth=1)
magnitude_ax.axvline(10, color="#666666", linestyle="--", linewidth=1)
magnitude_ax.set_title(r"Bode magnitude", fontsize=15, pad=6)
magnitude_ax.set_ylabel(r"$20\log_{10}|G(\mathrm{i}\omega)|$ (dB)", fontsize=12)
magnitude_ax.grid(True, which="both", alpha=0.35)

phase_ax.semilogx(
    frequency, phase, color="#0055A4", linewidth=1.8
)
phase_ax.set_title(r"Bode phase", fontsize=15, pad=6)
phase_ax.set_xlabel(r"Frequency $\omega$ (rad/s)", fontsize=12)
phase_ax.set_ylabel(r"Phase (degrees)", fontsize=12)
phase_ax.grid(True, which="both", alpha=0.35)

for axis in (step_ax, magnitude_ax, phase_ax):
    axis.tick_params(axis="both", labelsize=10)

fig.tight_layout(h_pad=1.0)
fig.savefig("ch06-higherorder-example.pdf", bbox_inches="tight")
plt.close(fig)
