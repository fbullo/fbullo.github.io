# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
import control as ctrl

# Matplotlib style (uses LaTeX if available in your environment)
plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"]
})

# -----------------------------
# Common settings
# -----------------------------
time_constants = [0.1, 0.3, 0.6, 1, 3, 6, 10]
frequencies = np.linspace(0, 10, 1500)  # rad/s (for simple magnitude plots)
colors = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']

# -----------------------------
# Low-pass: G(s) = 1 / (tau s + 1)
# -----------------------------
fig, ax = plt.subplots(figsize=(8, 5))
for idx, tau in enumerate(time_constants):
    mag = 1.0 / np.sqrt(1.0 + (tau * frequencies)**2)
    ax.plot(frequencies, mag, color=colors[idx], label=fr'$\tau={tau}$')

ax.set_title(r'Magnitude Response: $G(s)=\frac{1}{\tau s + 1}$ (Low-pass)')
ax.set_xlabel(r'Frequency $\omega$')
ax.set_ylabel(r'$|G(i\omega)|$')
ax.grid(True)
ax.legend()
ax.set_xlim(0, 8)
ax.set_ylim(0, 1.05)
plt.tight_layout()
plt.savefig("ch06-freqresponse-lowpass.pdf", bbox_inches='tight')
plt.close(fig)

# -----------------------------
# High-pass: G(s) = (tau s) / (tau s + 1)
# -----------------------------
fig, ax = plt.subplots(figsize=(8, 5))
for idx, tau in enumerate(time_constants):
    mag = (tau * frequencies) / np.sqrt(1.0 + (tau * frequencies)**2)
    ax.plot(frequencies, mag, color=colors[idx], label=fr'$\tau={tau}$')

ax.set_title(r'Magnitude Response: $G(s)=\frac{\tau s}{\tau s + 1}$ (High-pass)')
ax.set_xlabel(r'Frequency $\omega$')
ax.set_ylabel(r'$|G(i\omega)|$')
ax.grid(True)
ax.legend()
ax.set_xlim(0, 8)
ax.set_ylim(0, 1.05)
plt.tight_layout()
plt.savefig("ch06-freqresponse-highpass.pdf", bbox_inches='tight')
plt.close(fig)

# -----------------------------
# Bode magnitude (dB), tau = 1
# -----------------------------
tau = 1.0
omega = np.logspace(-2, 2, 500)  # rad/s

# Low-pass TF: G(s) = 1 / (s + 1)
sys_low = ctrl.TransferFunction([1.0], [tau, 1.0])
mag_low, phase_low, omega_low = ctrl.freqresp(sys_low, omega)
mag_low_db = 20.0 * np.log10(np.squeeze(mag_low))

fig, ax = plt.subplots(figsize=(7, 4))
ax.semilogx(omega_low, mag_low_db, color='#0055A4')
ax.set_title(r'Bode Magnitude (dB): $G(s)=\frac{1}{s + 1}$ (Low-pass, $\tau=1$)')
ax.set_xlabel(r'Frequency $\omega$ (rad/s)')
ax.set_ylabel('Magnitude (dB)')
ax.grid(True, which="both", ls="--")
plt.tight_layout()
plt.savefig("ch06-bode-lowpass.pdf", bbox_inches='tight')
plt.close(fig)

# High-pass TF: G(s) = s / (s + 1)
sys_high = ctrl.TransferFunction([tau, 0.0], [tau, 1.0])  # simplifies to s/(s+1) for tau=1
mag_high, phase_high, omega_high = ctrl.freqresp(sys_high, omega)
mag_high_db = 20.0 * np.log10(np.squeeze(mag_high))

fig, ax = plt.subplots(figsize=(7, 4))
ax.semilogx(omega_high, mag_high_db, color='#a43e00')
ax.set_title(r'Bode Magnitude (dB): $G(s)=\frac{s}{s + 1}$ (High-pass, $\tau=1$)')
ax.set_xlabel(r'Frequency $\omega$ (rad/s)')
ax.set_ylabel('Magnitude (dB)')
ax.grid(True, which="both", ls="--")
plt.tight_layout()
plt.savefig("ch06-bode-highpass.pdf", bbox_inches='tight')
plt.close(fig)

