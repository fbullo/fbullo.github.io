# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np; import matplotlib.pyplot as plt
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Define the functions
def exp_func(x):
    return np.exp(x)

def pade_approx(x):
    return (2 + x) / (2 - x)

def pade_approx2(x):
    # second-order Pade approximant: complex poles 3 +- i sqrt(3), no real asymptote
    return (12 + 6*x + x**2) / (12 - 6*x + x**2)

BROWN = '#d35000'   # gnbrown4: elements added 2026-09-24

# Generate values over a reasonable range
x = np.linspace(-3.0, 2.5, 1101)
y_exp = exp_func(x)
y_pade = pade_approx(x)
y_pade[np.abs(x - 2) < 0.02] = np.nan   # do not connect the two branches at y=2
y_pade2 = pade_approx2(x)

# Plot the two functions
plt.figure(figsize=(5.5, 2.4))

plt.plot(x, y_exp, label=r"$e^y$", linestyle='-', linewidth=2)
plt.plot(x, y_pade, label=r"first-order Pad\'e: $\frac{2 + y}{2 - y}$", linestyle='--', linewidth=2)
plt.plot(x, y_pade2, label=r"second-order Pad\'e: $\frac{12 + 6y + y^2}{12 - 6y + y^2}$", linestyle='-.', linewidth=2, color=BROWN)
plt.axvline(2, color=BROWN, lw=0.8, ls=':')   # vertical asymptote of the first-order approximant

# Highlight vertical asymptotes by limiting the plot range
plt.ylim(-1, 8)
plt.xlim(-3.0, 2.5)

# Labels and legend
plt.title(r"Comparison of $e^y$ and its Pad\'e approximants")
plt.xlabel("$y$")
plt.axhline(0, color='black', lw=0.5)
plt.axvline(0, color='black', lw=0.5)
plt.legend(loc='upper left', fontsize=8)
plt.grid(True)

# Save the plot to a PDF file
plt.savefig("ch04-pade-approximant.pdf", bbox_inches='tight')

# Close the plot to free memory
plt.close()
