# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
import control as ctrl
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Define the system parameters
tau = 2.0  # Time constant for the first-order system
Ki = 0.5   # Integral gain

# Define the range of proportional gains
Kp_values = [0.01, 0.1, 1, 10, 100]

# Define brown-like color range
colors = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']
arrow_color = '#0055A4'
text_color = arrow_color  

# Frequency vector for Bode plot
frequency_vector = np.logspace(-3, 3, 1000)  # From 10^-3 to 10^3 rad/s

# Time vector for step response
time_vector = np.linspace(0, 20, 1000)  # From 0 to 20 seconds

# Define the first order system (plant) G(s) = 1 / (tau * s + 1)
tfP = ctrl.TransferFunction([1], [tau, 1])

# Set up the figure for Bode and step response plots
plt.figure(figsize=(18, 12))

def tf_to_latex(tf):
    """Converts a control.TransferFunction object to a LaTeX string."""
    def poly_to_latex(poly):
        """Converts polynomial coefficients to a LaTeX polynomial string."""
        terms = []
        for i, coef in enumerate(poly):
            power = len(poly) - i - 1
            if coef != 0:
                if power > 0:
                    term = f"{coef} s^{{{power}}}"
                elif power == 0:
                    term = f"{coef}"
                terms.append(term)
        return ' + '.join(terms)

    num, den = tf.num[0][0], tf.den[0][0]  # Assuming SISO transfer function
    latex_str = f"\\frac{{{poly_to_latex(num)}}}{{{poly_to_latex(den)}}}"
    return latex_str


for i, title in enumerate(["Sensitivity Y/N", "Complementary Sensitivity -X/N", "Load->State X/D Sensitivity", "Noise->Control U/N Sensitivity"]):
    for Kp_idx, Kp in enumerate(Kp_values):
        # Define the PI controller PI(s) = Kp + Ki/s
        tfPI = ctrl.TransferFunction([Kp, Ki], [1, 0])

        # Compute the transfer functions
        tfL = tfP * tfPI  # Open-loop transfer function
        tfS = 1 / (1 + tfL)  # Sensitivity 
        tfT = tfL / (1 + tfL)  # Complementary sensitivity 
        tfCS = tfPI / (1 + tfL)  # Noise->Control Sensitivity
        tfLS = tfP / (1 + tfL)  # Load->State Sensitivity

        # Print the transfer functions in LaTeX format
        # print("Sensitivity Y/N:"); print(tf_to_latex(tfS))
        # print("Complementary Sensitivity -X/N:"); print(tf_to_latex(tfT))
        # print("Load->State X/D Sensitivity:"); print(tf_to_latex(tfLS))
        # print("Noise->Control U/N Sensitivity:"); print(tf_to_latex(tfCS))
        
        # Select the appropriate transfer function
        func = {'Sensitivity Y/N': tfS, 'Complementary Sensitivity -X/N': tfT, 'Load->State X/D Sensitivity': tfLS,
                'Noise->Control U/N Sensitivity': tfCS}[title]
        
        # Bode plot setup
        plt.subplot(4, 2, 2 * i + 1)
        color = colors[Kp_idx % len(colors)]
        
        # Compute and plot the step response of the transfer function
        T, yout = ctrl.step_response(func,  T=time_vector)
        plt.plot(T, yout, label=f'$k_p={Kp}$', color=color)
        plt.xlabel('Time [s]')
        plt.ylabel('Response')
        plt.title('Step Response: ' + title.replace('->', r'$\to$'))
        plt.grid();  plt.legend(); plt.xlim(0,20)


        # Check if it's the fourth time response plot (Noise->Control U/N Sensitivity)
        if title == "Noise->Control U/N Sensitivity" and Kp_idx == len(Kp_values) - 1:
            ax = plt.gca()
            ax.text(3.5, 60, "high $k_p$ implies large control", ha="center", va="center", rotation=0, size=10, color=text_color, bbox=dict(boxstyle="larrow,pad=0.3", fc="none", ec=arrow_color, lw=2, alpha=0.5))


        
        # Step response plot setup
        plt.subplot(4, 2, 2 * i + 2)

        
        # Plot Bode plot of the transfer function
        mag, phase, omega = ctrl.bode_plot(func, dB=True, omega=frequency_vector, plot=False)
        plt.semilogx(omega, 20 * np.log10(mag), label=f'$k_p={Kp}$', color=color)
        plt.xlabel('Frequency [rad/s]')
        plt.ylabel('Magnitude [dB]')
        plt.title('Bode: ' + title.replace('->', r'$\to$'))
        plt.grid(which='both')
        plt.legend(loc='center left')

        # Check if it's the fourth Bode plot (Noise->Control U/N Sensitivity)
        if title == "Noise->Control U/N Sensitivity" and Kp_idx == len(Kp_values) - 1:
            ax = plt.gca()
            # 2026-09-24: this label said "low k_p"; the data (+40 dB at k_p=100) and the caption say high
            ax.text(500, 0.01, "high $k_p$ implies noise amplification", ha="center", va="center", rotation=-45, size=10, color=text_color, bbox=dict(boxstyle="larrow,pad=0.3", fc="none", ec=arrow_color, lw=2, alpha=0.5))

        

plt.tight_layout()
plt.savefig('ch08-gangoffour-firstorder-pi.pdf')
