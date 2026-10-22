import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import odeint

# Logistic equation
def log_eq(x, t, r, kappa):
    return r * x * (1 - x / kappa)

# Parameters
r, kappa = 0.1, 100
initial_conditions = [1, 10, 50, 100]
times = np.linspace(0, 125, 1000)

# Solve and plot
plt.figure()
for x0 in initial_conditions:
    solution = odeint(log_eq, x0, times, args=(r, kappa))
    plt.plot(times, soluton)

plt.xlabel("Time"), plt.ylabel("Population")
plt.legend(), plt.show()

# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit
