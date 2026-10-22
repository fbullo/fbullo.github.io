# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import matplotlib.pyplot as plt
import numpy as np

# Different values of 'omegan' and their corresponding colors
omegan_values = [0.01, 0.1, 0.5, 1, 1.5, 2, 5]
colors = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']
plt.rcParams.update({"text.usetex": True, "font.family": "serif",
    "font.serif": ["Computer Modern Roman"] })

# Plot for short time domain (0 to 5)
t_short = np.linspace(0, 5, 100)
plt.figure(figsize=(8, 4))
for omegan, color in zip(omegan_values, colors):
    plt.plot(t_short, t_short * np.exp(-omegan * t_short), label=f'$\\omega_n =$ {omegan}', color=color)
plt.xlabel(r't')
plt.ylabel('t * exp(-omegan * t)')
plt.title(r'$y(t) = t e^{-\omega_{n} t}$, over time domain (0,5): ')
plt.xlim(0, 5); plt.ylim(0, 5); plt.grid(True);
plt.legend()
plt.savefig("ch05-secondorder-criticallydamped-shorttime.pdf", bbox_inches='tight')


# Plot for long time domain (0 to 100)
t_long = np.linspace(0, 200, 500)
plt.figure(figsize=(8, 4))
for omegan, color in zip(omegan_values, colors):
    plt.plot(t_long, t_long * np.exp(-omegan * t_long), label=f'$\\omega_n =$ {omegan}', color=color)
plt.xlabel('t')
plt.ylabel('t * exp(-omegan * t)')
plt.title(r'$y(t) = t e^{-\omega_{n} t}$, over time domain (0,200): ')
plt.xlim(0, 200); plt.ylim(0, 200); plt.grid(True);
plt.legend()
plt.savefig("ch05-secondorder-criticallydamped-longtime.pdf", bbox_inches='tight')
