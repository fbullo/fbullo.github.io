# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np; from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Define the system of ODEs with state = [xs, xs_dot, xu, xu_dot]
def system_of_eqns(t, state, mu, ms, ks, b, kw, road):
    xs, xs_dot, xu, xu_dot = state
    xs_ddot = (-ks*(xs-xu) - b*(xs_dot-xu_dot)) / ms
    xu_ddot = (ks*(xs-xu) + b*(xs_dot-xu_dot) - kw*xu + kw*road(t)) / mu
    return [xs_dot, xs_ddot, xu_dot, xu_ddot]

# Parameters for a "quarter automobile" and time array
ms = 400      # Sprung mass (for a quarter of a car)
mu = 45       # Unsprung mass
kw = 250000   # Wheel stiffness
ks = 26500    # Suspension stiffness
b  = 1950     # Suspension damping coefficient

# Initial conditions: [xs, xs_dot, xu, xu_dot]. positions in meters.
t = np.linspace(0, 3.0, 600); initial_conditions = [-0.1, 0.0, 0.00, 0.0]
sol = solve_ivp(system_of_eqns, (0, 3), initial_conditions, t_eval=t,
                args=(mu, ms, ks, b, kw, lambda t: 0), rtol=1e-8)

# Plotting the unforced solution
plt.figure(figsize=(10, 5)); plt.plot(t, sol.y[0], label='$x_s$ sprung')
plt.plot(t, sol.y[2], label='$x_{us}$ unsprung'); plt.grid(True)
plt.xlabel('time $t$',fontsize=16); plt.ylabel('position (meters)',fontsize=16); plt.xlim(0, 3.0);
plt.title('Unforced suspension system',fontsize=16); plt.legend();
plt.savefig("ch02-suspension-unforced.pdf", bbox_inches='tight')

# Road surface: zero for first .5 seconds, then a sinusoidal bump
bumpheight = .116   # typical bump height = .116 meters (about 4.6 inches)
duration = .46 / 4.4  # typical bump width = 18 inches = 0.46 meters. 10 miles/hour = 4.4 meter/sec
def bump_road(t):
    if 0.5 <= t < 0.5 + duration:
        return bumpheight * np.sin((t - .5) * np.pi / duration)
    else:
        return 0

# Forced case from rest; max_step keeps the solver from stepping over the bump
sol_forced = solve_ivp(system_of_eqns, (0, 3), [0, 0, 0, 0], t_eval=t,
    max_step=duration/20, args=(mu, ms, ks, b, kw, bump_road))
road_data = np.array([bump_road(time) for time in t])

# Plotting the forced solution
plt.figure(figsize=(10, 5)); plt.plot(t, sol_forced.y[0], label='$x_s$ sprung mass')
plt.plot(t, sol_forced.y[2], label='$x_{us}$ unsprung mass'); plt.grid(True)
plt.plot(t, road_data, label='$r$ road surface', linestyle='--'); plt.xlim(0, 3.0);
plt.xlabel('time $t$',fontsize=16); plt.ylabel('position (meters)',fontsize=16); plt.title('Forced suspension system',fontsize=16)
plt.legend(); plt.savefig("ch02-suspension-forced.pdf", bbox_inches='tight')
