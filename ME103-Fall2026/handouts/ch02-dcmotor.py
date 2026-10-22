# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np; from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt

def motor_dynamics(t, state, I_m, b, K_torque, L, R, K_velocity, V_input):
    theta_m, theta_m_dot, ic = state
    theta_m_ddot = (K_torque * ic - b * theta_m_dot) / I_m
    ic_dot = (V_input(t) - K_velocity * theta_m_dot - R * ic) / L
    return [theta_m_dot, theta_m_ddot, ic_dot]

# Parameters for the DC motor
I_m = 0.01         # Moment of inertia of the motor
b = 0.1            # Damping coefficient
K_torque = 0.01    # Torque constant
L = 0.5            # Motor inductance
R = 1              # Motor resistance
K_velocity = 0.01  # Back EMF constant

# Time array
t = np.linspace(0, 6, 1000)

# Voltage input: step function at 1V
V_input = lambda t: 1.0 if t > 1 else 0.0

# Initial conditions: [theta_m, theta_m_dot, ic]
initial_conditions = [0.0, 0.0, 0.0]
sol = solve_ivp(motor_dynamics, [t[0], t[-1]], initial_conditions, t_eval=t, args=(I_m, b, K_torque, L, R, K_velocity, V_input))

# Plotting
plt.figure(figsize=(8, 6)); plt.subplot(3, 1, 1); plt.xlim(0, 6)
plt.plot(sol.t, sol.y[0], label='Motor Position (rad)', color='#0055A4')
plt.grid(True); plt.ylabel('Position (rad)'); plt.legend()

plt.subplot(3, 1, 2)
plt.plot(sol.t, sol.y[1], label='Motor Speed (rad/s)', color='#a43e00'); plt.xlim(0, 6)
plt.grid(True); plt.ylabel('Speed (rad/s)'); plt.legend()

plt.subplot(3, 1, 3)
plt.plot(sol.t, sol.y[2], label='Current (A)', color='#ff6100'); plt.xlim(0, 6)
plt.grid(True); plt.xlabel('Time'); plt.ylabel('Current (A)'); plt.legend()

plt.tight_layout()
plt.savefig("ch02-dcmotor.pdf", bbox_inches='tight')
