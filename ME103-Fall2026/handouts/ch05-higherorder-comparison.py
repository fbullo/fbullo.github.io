# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import control as ctrl
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1.inset_locator import inset_axes


plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"]
})

def draw_complex_plane_axes(ax, xrange=(-10, 2), yrange=(-6, 6)):
    """Configure complex plane axes with arrowheads at ends"""
    ax.axhline(0, color='black', lw=1)
    ax.axvline(0, color='black', lw=1)
    ax.set_xlim(xrange)
    ax.set_ylim(yrange)
    # ax.set_aspect('equal')

    # Correct arrow placement using axis transforms (not xrange/yrange!)
    ax.plot(1, 0, ">k", transform=ax.get_yaxis_transform(), clip_on=False)
    ax.plot(0, 1, "^k", transform=ax.get_xaxis_transform(), clip_on=False)

    ax.set_xticks([])
    ax.set_yticks([])
    ax.tick_params(axis='both', which='both', length=0)
    for spine in ax.spines.values():
        spine.set_visible(True)

def draw_poles(ax, poles, dominant_indices=[]):
    """Draw poles on the given axis, highlighting dominant poles"""
    for i, (real, imag) in enumerate(poles):
        color = oranges[3] if i in dominant_indices else blues[4]
        ax.plot(real, imag, 'x', color=color, markersize=10, markeredgewidth=2)

# Define the requested colors from darker to lighter shades
blues = ['#002447', '#003c76', '#0055A4', '#006CD4', '#0085ff', '#239cff', '#58b1ff']
oranges = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']


# --------------------- STUDY 1 ---------------------
poles = [-1, -5 + 5j, -5 - 5j, -7, -9]
denominator = np.poly(poles)
numerator_higher_order = [denominator[-1]]

system_higher_order = ctrl.TransferFunction(numerator_higher_order, denominator)
first_order_system = ctrl.TransferFunction([1], [1, 1])

time_higher_order, response_higher_order = ctrl.step_response(system_higher_order)
time_first_order, response_first_order = ctrl.step_response(first_order_system)

# Plot Study 1
######## fig1, ax1 = plt.subplots(figsize=(10, 6))
## fig1, ax1 = plt.subplots(figsize=(5.6, 3.2))
fig1, ax1 = plt.subplots(figsize=(8, 4.8)); ax1.set_xlim((0,7))
ax1.plot(time_first_order, response_first_order, color=oranges[3])
ax1.plot(time_higher_order, response_higher_order, color=blues[4])
ax1.set_xlabel("time $t$", fontsize=16)
ax1.set_title("Step response of higher-order system vs first-order system", fontsize=16)
ax1.grid(True); ax1.set_ylim([0, 1.25])
ax1.plot(time_first_order, response_first_order, color=oranges[3],
         label=r"First-order system, pole at $-1$")
ax1.plot(time_higher_order, response_higher_order, color=blues[4],
         label=r"Higher-order system, poles at $-1,\,-5\pm5\mathrm{i},\,-7,\,-9$")
ax1.legend(loc='lower right', fontsize=14);


# Add inset for Study 1
axins1 = inset_axes(ax1, width="40%", height="40%", loc='center right', borderpad=2)
draw_complex_plane_axes(axins1)
draw_poles(axins1, [(p.real, p.imag) for p in poles], dominant_indices=[0])  # Pole at -1 is dominant
axins1.set_title("Pole diagram", fontsize=14, pad=6)

# Add this after creating axins1 and drawing poles for Study 1:

# Vertical dashed lines at x=-1 and x=-5 in the inset
axins1.axvline(x=-1, color='#666666', linestyle='--', linewidth=0.8, alpha=0.7)
axins1.axvline(x=-5, color='#666666', linestyle='--', linewidth=0.8, alpha=0.7)

# Horizontal dashed line from x=-5 to x=-1 with arrows in the inset
y_line = -3  # Position the line in the upper part of the complex plane
axins1.annotate('', xy=(-1, y_line), xytext=(-5, y_line), 
                arrowprops=dict(arrowstyle='<->', color='#666666', lw=0.8, linestyle='--'))

# Add the "5×" text above the horizontal line
axins1.text(-3, y_line + 0.1, r'$5\times$', ha='center', va='bottom', fontsize=10)
axins1.text(-1, -1.5, r'dominant pole', ha='center', va='bottom', fontsize=10, color = '#d35000')


fig1.savefig("ch05-higherorder-comparison-onepole.pdf", bbox_inches='tight')
plt.close(fig1)

# --------------------- STUDY 2 ---------------------
poles_higher_order_2 = [-1 + 2j, -1 - 2j, -5, -7, -9]
poles_second_order = [-1 + 2j, -1 - 2j]

denominator_higher_order_2 = np.poly(poles_higher_order_2)
denominator_second_order = np.poly(poles_second_order)

numerator_higher_order_2 = [denominator_higher_order_2[-1]]
numerator_second_order = [denominator_second_order[-1]]

system_higher_order_2 = ctrl.TransferFunction(numerator_higher_order_2, denominator_higher_order_2)
system_second_order = ctrl.TransferFunction(numerator_second_order, denominator_second_order)

time_higher_order_2, response_higher_order_2 = ctrl.step_response(system_higher_order_2)
time_second_order, response_second_order = ctrl.step_response(system_second_order)

# Plot Study 2
#### fig2, ax2 = plt.subplots(figsize=(10, 6))
fig2, ax2 = plt.subplots(figsize=(8, 4.8)); ax2.set_xlim((0,7))
ax2.plot(time_second_order, response_second_order, color=oranges[3])
ax2.plot(time_higher_order_2, response_higher_order_2, color=blues[4])
ax2.set_title("Step response of higher-order system vs second-order system", fontsize=16)
ax2.set_xlabel("time $t$", fontsize=16)
ax2.grid(True); ax2.set_ylim([0, 1.25])
ax2.plot(time_second_order, response_second_order, color=oranges[3],
         label=r"Second-order system, poles at $-1\pm2\mathrm{i}$")
ax2.plot(time_higher_order_2, response_higher_order_2, color=blues[4],
         label=r"Higher-order system, poles at $-1\pm2\mathrm{i},\,-5,\,-7,\,-9$")
ax2.legend(loc='lower right', fontsize=14)

# Add inset for Study 2
axins2 = inset_axes(ax2, width="40%", height="40%", loc='center right', borderpad=2)
draw_complex_plane_axes(axins2)
draw_poles(axins2, [(p.real, p.imag) for p in poles_higher_order_2], dominant_indices=[0, 1])  # Dominant pair
axins2.set_title("Pole diagram", fontsize=12, pad=6)

# Vertical dashed lines at x=-1 and x=-5 in the inset
axins2.axvline(x=-1, color='#666666', linestyle='--', linewidth=0.8, alpha=0.7)
axins2.axvline(x=-5, color='#666666', linestyle='--', linewidth=0.8, alpha=0.7)

# Horizontal dashed line from x=-5 to x=-1 with arrows in the inset
y_line = -5  # Position the line in the upper part of the complex plane
axins2.annotate('', xy=(-1, y_line), xytext=(-5, y_line), 
                arrowprops=dict(arrowstyle='<->', color='#666666', lw=0.8, linestyle='--'))

# Add the "5×" text above the horizontal line
axins2.text(-3, y_line+.2, r'$5\times$', ha='center', va='bottom', fontsize=10)
axins2.text(-1, -3.5, r'dominant poles', ha='center', va='bottom', fontsize=10, color = '#d35000')

fig2.savefig("ch05-higherorder-comparison-twopoles.pdf", bbox_inches='tight')
plt.close(fig2)

