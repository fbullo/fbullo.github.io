# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
import control as ctrl

# ----------------------------
# Parameters (edit as needed)
# ----------------------------
M = 1.0      # main mass
K = 4.1      # main spring
m = 0.10     # absorber mass
k = 0.40     # absorber spring; sqrt(k/m) = 2 rad/s
P0 = 1.0     # force amplitude for time simulation (arbitrary scale)

# Natural frequencies
omega_n_main = np.sqrt(K / M)
omega_abs = np.sqrt(k / m)  # tuned frequency of the absorber

# Frequency grid for Bode magnitude plot
omega = np.logspace(-2, 2, 1000)  # rad/s

# ------------------------------------
# Transfer functions X1/P (magnitude)
# ------------------------------------
# Without absorber: M x1dd + K x1 = P  => X1/P = 1 / (M s^2 + K)
G_no = ctrl.TransferFunction([1.0], [M, 0.0, K])

# With absorber:
# Equations:
#   (M s^2 + K + k) X1 - k X2 = P
#   -k X1 + (m s^2 + k) X2 = 0  =>  X2 = (k/(m s^2 + k)) X1
# => X1/P = (m s^2 + k) / [ (M s^2 + K + k)(m s^2 + k) - k^2 ]
num_with = [m, 0.0, k]
den_with = np.polymul([M, 0.0, K + k], [m, 0.0, k])  # (M s^2 + (K+k))*(m s^2 + k)
den_with[-1] -= k**2                                 # subtract k^2
G_with = ctrl.TransferFunction(num_with, den_with)

# Frequency responses (complex), then magnitude in dB
mag_no, ph_no, om_no = ctrl.freqresp(G_no, omega)
mag_with, ph_with, om_with = ctrl.freqresp(G_with, omega)

mag_no_db = 20 * np.log10(np.abs(mag_no.squeeze()))
mag_with_db = 20 * np.log10(np.abs(mag_with.squeeze()))

# ------------------------------------
# Plot: Bode magnitude comparison
# ------------------------------------
fig, ax = plt.subplots(figsize=(7.5, 4.5))
ax.semilogx(omega, mag_no_db, label='without absorber', lw=1.8)
ax.semilogx(omega, mag_with_db, label='with absorber', lw=1.8)

# Mark key frequencies
ax.axvline(omega_n_main, ls='--', lw=1.0, color='#666666')
ax.text(
    omega_n_main, ax.get_ylim()[0] + 3,
    r'$\sqrt{K/M}=2.025$', rotation=90,
    va='bottom', ha='right', fontsize=9,
)

ax.axvline(omega_abs, ls='--', lw=1.0, color='#0055A4')
ax.text(
    omega_abs, ax.get_ylim()[0] + 3,
    r'$\omega=\sqrt{k/m}=2$', rotation=90,
    va='bottom', ha='left', color='#0055A4', fontsize=9,
)

ax.set_title(r'Vibration absorber: main-mass displacement')
ax.set_xlabel(r'frequency $\omega$ (rad/s)')
ax.set_ylabel(r'$20\log_{10}|X_1/P|$ (dB)')
ax.grid(True, which='both', ls='--', lw=0.6, alpha=0.7)
ax.legend()
plt.tight_layout()
plt.savefig('ch06-vibration-absorber-bode.pdf', bbox_inches='tight')
plt.close(fig)

# ---------------------------------------------------------
# Optional: time response at tuned frequency (x1 ~ 0 shown)
# ---------------------------------------------------------
# Build state-space for the 2-DOF undamped system under external force P0*sin(omega_abs t)
# States: [x1, v1, x2, v2]
# x1dot = v1
# v1dot = (1/M)*( -K*x1 - k*(x1 - x2) + P0*sin(omega_abs*t) )
# x2dot = v2
# v2dot = (1/m)*( -k*(x2 - x1) )

# We will use a time-varying input u(t)=sin(omega_abs t) scaled by P0
# Represent as an external input shaping matrix B so that input is scalar u(t),
# and scale by P0 inside u(t) definition.

A = np.array([
    [0,        1,        0,       0],
    [-(K + k)/M, 0,     k/M,      0],
    [0,        0,        0,       1],
    [k/m,      0,     -k/m,       0]
], dtype=float)

B = np.array([[0], [1.0/M], [0], [0]], dtype=float)  # input enters as force on mass M
C = np.eye(4)  # we will observe all states then extract x1,x2
D = np.zeros((4, 1))

sys_ss = ctrl.ss(A, B, C, D)

# Time grid and input
t = np.linspace(0, 60, 4000)
u = P0 * np.sin(omega_abs * t)

# Initial conditions from the analytical tuned solution with a1=0 and a2=-P0/k:
# x1(0)=0, v1(0)=0, x2(0)=0, v2(0) = a2*omega_abs*cos(0) = -(P0/k)*omega_abs
x0 = np.array([0.0, 0.0, 0.0, -(P0 / k) * omega_abs])

tout, yout, xout = ctrl.forced_response(sys_ss, T=t, U=u, X0=x0, return_x=True)
x1 = yout[0, :]
x2 = yout[2, :]

fig, ax = plt.subplots(figsize=(7.5, 4.0))
ax.plot(tout, x1, label=r'$x_1(t)$ (main mass)', lw=1.6)
ax.plot(tout, x2, label=r'$x_2(t)$ (absorber mass)', lw=1.2, alpha=0.85)
ax.set_title(r'Time response at $\omega=\sqrt{k/m}$ (tuned)')
ax.set_xlabel('time t')
ax.set_ylabel('displacement')
ax.grid(True, ls='--', lw=0.6, alpha=0.7)
ax.set_xlim(0, t[-1])
ax.legend()
plt.tight_layout()
plt.savefig('ch06-vibration-absorber-time.pdf', bbox_inches='tight')
plt.close(fig)
