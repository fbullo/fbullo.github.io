# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

# Python libraries
import numpy as np
import matplotlib.pyplot as plt

plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"],
    "font.size": 16
})

# Parameters
a = 2        # amplitude
k = 1.5      # slope scale
s = 2        # so that tanh(ln(s)) = 0.5

# Define the nonlinear function and its linear approximation
def scaledTanh(x):
    return a * np.tanh(k * x)

def tangent_line(x):
    return a * k * x  # derivative at x=0

# Define x-range
x = np.linspace(-2, 2, 400)

# Tick and marker values
xtick_val = np.log(s) / k
ytick_val = 3 * a / 5

# Plotting
plt.figure(figsize=(8, 3))
plt.plot(x, scaledTanh(x), label=r"$a\,\tanh(k x)$", color="#0055A4", linewidth=2)
plt.plot(x, tangent_line(x), label=r"tangent at $x=0$: $akx$", color="#a43e00", linestyle='--', linewidth=2)

# Mark origin
plt.scatter(0, 0, color="black", zorder=5)

# Add blue scatter points at ±(ln2/k, 3a/5)
plt.scatter([-xtick_val, xtick_val], [-ytick_val, ytick_val], color="#0055A4", zorder=5)

# Labels and appearance
plt.xlabel(r"$x$")
plt.xticks([-xtick_val, xtick_val],
           [r"$-\frac{\ln 2}{k}$", r"$\frac{\ln 2}{k}$"])
plt.yticks([-a, -ytick_val, 0, ytick_val, a],
           [r"$-a$", r"$-\frac{3a}{5}$", r"$0$", r"$\frac{3a}{5}$", r"$a$"])
plt.grid(True)
plt.legend()
plt.xlim(-2, 2)
plt.ylim(-2.5, 2.5)

# Save to PDF
plt.savefig("ch03-plot-tanh.pdf", bbox_inches="tight")


