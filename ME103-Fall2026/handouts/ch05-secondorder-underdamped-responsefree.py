# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt

plt.rcParams.update({"text.usetex": True, "font.family": "serif",
    "font.serif": ["Computer Modern Roman"] })

# Define the requested colors
blues = ['#002447', '#003c76', '#0055A4', '#006CD4', '#0085ff', '#239cff', '#58b1ff']
oranges = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']

# Common parameters
x_0 = 1
omegan = 1
T_func = lambda zeta: 1 / (zeta * omegan)  # τ function

# Function x(t)
def x_t_func(t, zeta, omegan):
    omegad = omegan * np.sqrt(1 - zeta**2)
    return x_0 * np.exp(-zeta * omegan * t) * (
        np.cos(omegad * t) + (zeta / np.sqrt(1 - zeta**2)) * np.sin(omegad * t))

# Envelope function
def envelope_func(t, zeta):
    return (x_0 / np.sqrt(1 - zeta**2)) * np.exp(-zeta * omegan * t)

# Generate and save plot
def generate_plot(zeta, filename):
    T = T_func(zeta)  # Calculate τ
    t = np.linspace(0, 6 * T, 1000)  # Time range
    x_t = x_t_func(t, zeta, omegan)  # x(t) values
    envelope = envelope_func(t, zeta)  # Envelope values

    plt.figure(figsize=(5.6, 3.2))
    
    # Plot x(t) with blues[4]
    plt.plot(t, x_t, label=r"$x(t) = x_0 e^{-\zeta \omega_n t} \left( \cos(\omega_d t) + \frac{\zeta}{\sqrt{1 - \zeta^2}} \sin(\omega_d t) \right)$", color=blues[4])

    # Plot positive and negative envelopes with oranges[4]
    plt.plot(t, envelope, '--', label=r"envelope = $\pm \frac{x_0}{\sqrt{1 - \zeta^2}} e^{-\zeta \omega_n t}$", color=oranges[4])
    plt.plot(t, -envelope, '--', color=oranges[4])

    # Plot smaller black dots at ±1/(sqrt(1 - zeta^2))
    dot_value = 1 / np.sqrt(1 - zeta**2)
    plt.plot(0, 1, 'ko', markersize=3)  # Positive dot
    plt.plot(0, dot_value, 'ko', markersize=3)  # Positive dot
    plt.plot(0, -dot_value, 'ko', markersize=3)  # Negative dot

    # Add text labels for the initial envelope values
    plt.text(0.3, (.8*dot_value+.2) , rf"$+\frac{{x_0}}{{\sqrt{{1 - \zeta^2}}}}$", fontsize=12, color="black")
    plt.text(0.3, -(.8*dot_value+.2) , rf"$-\frac{{x_0}}{{\sqrt{{1 - \zeta^2}}}}$", fontsize=12, color="black")

    # Custom ticks on the x-axis with LaTeX-style τ
    plt.xticks([n * T for n in range(1, 7)], [rf"${n}\tau$" for n in range(1, 7)])

    # Custom y-ticks with LaTeX labels
    plt.yticks([-x_0, -x_0 / 2, 0, x_0 / 2, x_0], [r"$-x_0$", "", "0", "", r"$x_0$"])

    # Adjusting xlim and ylim
    plt.xlim(0, 6 * T)
    plt.ylim(-1.7, 1.7)

    # Plot vertical and horizontal axes
    plt.axhline(0, color='black', linewidth=1)  # Horizontal axis
    plt.axvline(0, color='black', linewidth=1)  # Vertical axis

    # Labels and legend with added τ definition
    plt.xlabel(r"time (t)")
    # plt.ylabel(r"x(t)")
    #    plt.legend(loc="upper right", title=rf"$\tau = \frac{{1}}{{\zeta \omega_n}}$")
    plt.legend(loc="upper right", title=rf"$\tau = \frac{{1}}{{\zeta \omega_n}}, \, \zeta = {zeta:.2f}$"
)
    plt.grid(True)

    # Adjust layout and save the figure
    plt.tight_layout()
    plt.savefig(filename, bbox_inches='tight')
    plt.close()

####  Generate plots for zeta = 0.35 and zeta = 0.75
# generate_plot(0.30, "2ndorder-underdamped-responsefree.30.pdf")
# generate_plot(0.35, "2ndorder-underdamped-responsefree.35.pdf")
generate_plot(0.40, "ch05-secondorder-underdamped-responsefree-40.pdf")
# generate_plot(0.45, "2ndorder-underdamped-responsefree.45.pdf")
#generate_plot(0.65, "2ndorder-underdamped-responsefree.65.pdf")
#generate_plot(0.75, "2ndorder-underdamped-responsefree.75.pdf")
generate_plot(0.80, "ch05-secondorder-underdamped-responsefree-80.pdf")
#generate_plot(0.85, "2ndorder-underdamped-responsefree.85.pdf")

