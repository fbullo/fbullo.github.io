# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

# Python libraries
import numpy as np; import matplotlib.pyplot as plt; from scipy.integrate import solve_ivp
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Differential equation model of the dynamical system
def model(t, v, b, m, f):
    dvdt = - (b/m) * v + f(t)/m
    return dvdt

# Parameters and time array
b = 4; m = 3; t = np.linspace(0, 10, 500)
# Force with a step change at time 5
def f(t):
    if t < 5:   
        return 20
    else: 
        return 30

# Initial conditions
v0_values = [2, 3, 4, 5, 6, 7, 8]; 

# Numerically integrate and plot solutions for each initial condition. 
plt.figure(figsize=(7, 4.2));  # Colors are (from darker to lighter)
oranges = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56'] 
for idx, v0 in enumerate(v0_values):
    v = solve_ivp(model, (0, 10), [v0], t_eval=t, args=(b, m, f), rtol=1e-8).y[0]
    plt.plot(t, v, label=f'$v_0={v0}$', color=oranges[idx])
   
# Annotate and save the plot
plt.title('Solutions to the car velocity system',fontsize=14)
plt.xlabel('time $t$', fontsize=16); plt.ylabel('state $v(t)$', fontsize=16); plt.legend(); plt.grid(True);
plt.xlim(0, 10); plt.savefig("ch02-first-order-ode.pdf", bbox_inches='tight')

# Second figure: Illustrate time constant
tau = m / b;  v0 = 1
def fzero(t):   
    return 0

# Numerically integrate and plot solution for the given initial condition
plt.figure(figsize=(10, 3));
v = solve_ivp(model, (0, 10), [v0], t_eval=t, args=(b, m, fzero), rtol=1e-8).y[0]
plt.plot(t, v, label=f'v0={v0}', color='#0085ff', zorder=2)

# Add tangent line at t=0. For x(t) = e^(-t/tau), the derivative at t=0 is dx/dt|_{t=0} = -1/tau
# So the tangent line is: y = x(0) + (dx/dt|_{t=0}) * t = 1 - t/tau
t_tangent = np.linspace(0, 2*tau, 100); tangent_line = v0 - t_tangent/tau
plt.plot(t_tangent, tangent_line, color=oranges[2], linestyle='--', linewidth=1.5, 
         label='Tangent at $t=0$', zorder=1)

# Highlighting the time constants
time_constants = [tau, 2*tau, 3*tau, 4*tau, 5*tau]
for tc in time_constants:
    plt.axvline(x=tc, color=oranges[4], linestyle='--', linewidth=0.75)
plt.xticks(time_constants, [r'${} \tau$'.format(int(tc/tau)) for tc in time_constants])

# Drawing the 1% horizontal dashed line
one_percent_value = 0.01; exp_value = np.exp(-1);
plt.axhline(y=one_percent_value, color=oranges[2], linestyle='--', linewidth=1.5)
plt.annotate(r'$1\% > \mathrm{e}^{-5} \approx 0.67\%$', (6*tau, one_percent_value), textcoords="offset points", xytext=(0,10), ha='left', color=oranges[2])
plt.axhline(y=exp_value, color=oranges[2], linestyle='--', linewidth=1.5)
plt.annotate(r'$\mathrm{e}^{-1}\approx 36.8\%$', (6*tau, exp_value), textcoords="offset points", xytext=(0,10), ha='left', color=oranges[2])

# Annotate and save the plot
plt.title(r'Solution to unforced first-order system: $\quad\tau \dot{x} = -x$',fontsize=11)
plt.xlabel('time $t$',fontsize=11); plt.ylabel('state $x(t)$',fontsize=11);
plt.grid(True); plt.xlim(0, 6); plt.ylim(-0, 1)
plt.savefig("ch02-first-order-ode-timeconstant.pdf", bbox_inches='tight')
