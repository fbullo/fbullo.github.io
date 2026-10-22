# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import odeint
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

def lotka_volterra(z, t, alpha, beta, gamma, delta):
    x, y = z
    dxdt = alpha * x - beta * x * y
    dydt = -delta * y + gamma * x * y
    return [dxdt, dydt]

alpha = 0.1; beta = 0.02; gamma = 0.01; delta = 0.1

# Define a function to save the plot to a file
def save_plot(points, filename):
    vectors = [lotka_volterra(point, 0, alpha, beta, gamma, delta) for point in points]
    U = [vec[0] for vec in vectors]
    V = [vec[1] for vec in vectors]
    X = [point[0] for point in points]
    Y = [point[1] for point in points]
    
    plt.figure(figsize=(5, 2.5))
    plt.quiver(X, Y, U, V, angles='xy', scale_units='xy', scale=0.5, width=0.004, pivot='middle', color='#0085ff')
    plt.xlim(0, 25);   plt.ylim(0, 15)
    plt.xlabel('prey population');   plt.ylabel('predator population')
    plt.yticks([0, 2.5, 5, 7.5, 10, 12.5, 15])   # <--- custom ticks here
    plt.grid(True)
    plt.savefig(filename, bbox_inches='tight')
    plt.close()

# Initial 4 points
points1 = [(5,6), (20,6), (10,2), (10,10)]
save_plot(points1, "ch01-howto-phaseportrait-1.pdf")

# Add 8 more points for a total of 12
points2 = points1 + [(5,2), (5,4), (5,6), (5,8), (5,10), (5,12),
                     (10,2), (10,4), (10,6), (10,8), (10,10), (10,12),
                     (15,2), (15,4), (15,6), (15,8), (15,10), (15,12),
                     (20,2),(20,4),(20,6),(20,8),(20,10),(20,12)]
save_plot(points2, "ch01-howto-phaseportrait-2.pdf")

# Add 30 random points for a total of 42
random_points_x = np.random.uniform(0, 25, 50)
random_points_y = np.random.uniform(0, 15, 50)
random_points = list(zip(random_points_x, random_points_y))
points3 = points2 + random_points
save_plot(points3, "ch01-howto-phaseportrait-3.pdf")


