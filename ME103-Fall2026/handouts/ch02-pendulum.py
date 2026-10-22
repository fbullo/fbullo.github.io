# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

# Python libraries
import numpy as np; import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Pendulum dynamics
def pendulum(t, Y, g, ell):
    theta, omega = Y; dtheta = omega; domega = -g/ell * np.sin(theta)
    return [dtheta, domega]

# Parameters and time array
g   = 9.81    # gravity
ell = 1.0     # length of the pendulum
m   = 0.5     # mass (not directly used in the equations, but provided for completeness)
t = np.linspace(0, 10, 1000)

# Initial conditions: [theta0, omega0] and plot the solution
initial_conditions = [[.1*np.pi, 0], [.4*np.pi, 0], [.7*np.pi, 0], [.99*np.pi, 0]]
colors = ['#752d00', '#a43e00', '#d35000', '#ff6100']
plt.figure(figsize=(6, 3))

for idx, ic in enumerate(initial_conditions):
    Y = solve_ivp(pendulum, (0, 10), ic, t_eval=t, args=(g, ell), rtol=1e-8).y
    plt.plot(t, Y[0], label=f'theta0={ic[0]:.2f}, omega0={ic[1]:.2f}', color=colors[idx])

# Set y-ticks to be fractions of pi
plt.yticks( [-np.pi, -np.pi/2, 0, np.pi/2, np.pi],
            [r'$-\pi$', r'$-\pi/2$', '0', r'$\pi/2$', r'$\pi$'] )
plt.title('Undamped pendulum dynamics ($\\theta(t)$ vs $t$) and phase portrait',fontsize=12)
plt.xlabel('time $t$',fontsize=12); plt.ylabel('$\\theta(t)$',fontsize=12); plt.xlim(0, 10);
plt.grid(True); plt.savefig("ch02-pendulum.pdf", bbox_inches='tight')

# Phase portrait
theta_range, omega_range = np.meshgrid(np.linspace(-2*np.pi, 2*np.pi, 20), np.linspace(-7, 7, 20))
dtheta, domega = pendulum(0, [theta_range, omega_range], g, ell)
magnitude = np.sqrt(dtheta**2 + domega**2)/2; plt.figure(figsize=(12,6));
plt.streamplot(theta_range, omega_range, dtheta, domega, density=.5, linewidth=magnitude, color='#0085ff' , broken_streamlines=False, arrowsize=3)

# Plotting the trajectories in the phase portrait
for idx, ic in enumerate(initial_conditions):
    Y = solve_ivp(pendulum, (0, 10), ic, t_eval=t, args=(g, ell), rtol=1e-8).y
    plt.plot(Y[0], Y[1], color=colors[idx], label=f'theta0={ic[0]:.2f}, omega0={ic[1]:.2f}')

# Plotting the scatter points at theta = -2pi, -pi, 0, pi, 2pi
for scatter_theta in [-2*np.pi, -np.pi, 0, np.pi, 2*np.pi]:
    plt.scatter(scatter_theta, 0, color='black', s=50, zorder=5)
plt.xticks([-2*np.pi, -3*np.pi/2, -np.pi, -np.pi/2, 0, np.pi/2, np.pi, 3*np.pi/2, 2*np.pi],
    [r'$-2\pi$', r'$-3\pi/2$', r'$-\pi$', r'$-\pi/2$', '0', r'$\pi/2$', r'$\pi$', r'$3\pi/2$', r'$2\pi$'])
plt.xlabel('$\\theta$',fontsize=24); plt.ylabel(r'$\omega$',fontsize=24); plt.xlim([-2*np.pi, 2*np.pi]); plt.ylim([-7, 7])
plt.tick_params(axis='both', which='major', labelsize=24)
plt.grid(True); plt.savefig("ch02-pendulum-phase.pdf", bbox_inches='tight')
