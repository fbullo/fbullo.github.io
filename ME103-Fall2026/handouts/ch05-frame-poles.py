# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import matplotlib.pyplot as plt
import numpy as np

## if you will need AMS fonts, like \mathbb, say: ax.text(1.25*R_circle, 1.0*R_circle, r"$\mathbb{C}$", fontsize=fs_C, color=col)
plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"],
    "text.latex.preamble": r"\usepackage{amsmath,amssymb,amsfonts}"
})

def draw_left_semicircle(ax, radius, **kwargs):
    """
    Draw a dashed semicircle of given radius centered at the origin,
    covering the left half-plane (from +i*radius to -i*radius).
    """
    r = abs(radius)
    theta = np.linspace(np.pi/2, 3*np.pi/2, 400)  # left semicircle
    x = r * np.cos(theta)
    y = r * np.sin(theta)
    ax.plot(x, y, linestyle="--", color="black", lw=0.5, **kwargs)


    
def create_complex_plane_axes(xrange=(-3, 1), yrange=(-2, 2)):
    """Create and configure the complex plane axes with custom limits"""
    fig, ax = plt.subplots(figsize=(2, 2))
    
    # Axes lines
    ax.axhline(0, color='black', lw=1)
    ax.axvline(0, color='black', lw=1)
    ax.set_xlim(xrange)
    ax.set_ylim(yrange)
    ax.set_aspect('equal')

    # Arrowheads
    ax.plot(1, 0, ">k", transform=ax.get_yaxis_transform(), clip_on=False)
    ax.plot(0, 1, "^k", transform=ax.get_xaxis_transform(), clip_on=False)

    # Remove ticks and box
    ax.set_xticks([])
    ax.set_yticks([])
    ax.tick_params(axis='x', which='both', length=0)
    ax.tick_params(axis='y', which='both', length=0)
    for spine in ax.spines.values():
        spine.set_visible(False)

    return fig, ax

def draw_poles(ax, poles, show_ticks=False):
    """Draw given poles on the axes, optionally add reference lines and labeled ticks"""
    x_ticks = []
    x_labels = []
    y_ticks = []
    y_labels = []

    for real, imag in poles:
        # Plot the pole only (no conjugate)
        ax.plot(real, imag, 'x', color='#0055A4', markersize=10, markeredgewidth=2)

        if show_ticks:
            # Vertical reference line
            if real not in x_ticks:
                ax.axvline(real, color='black', linestyle='--', lw=0.5)
                x_ticks.append(real)
                x_labels.append(f"${real}$")

            # Horizontal reference line
            if imag != 0 and imag not in y_ticks:
                ax.axhline(imag, color='black', linestyle='--', lw=0.5)
                y_ticks.append(imag)
                y_labels.append(r"$\mathrm{i}$" if imag > 0 else r"$-\mathrm{i}$")

    if show_ticks:
        ax.set_xticks(x_ticks)
        ax.set_xticklabels(x_labels)
        ax.set_yticks(y_ticks)
        ax.set_yticklabels(y_labels)


############# Plots

## First order pole with 1/\tau label
poles_to_plot = [(-1, 0)]
fig, ax = create_complex_plane_axes(xrange=(-2, 1), yrange=(-1, 1))
draw_poles(ax, poles_to_plot, show_ticks=False)
ax.text(-1, -.5, r'$-\frac{1}{\tau}$', va='center', ha='center', fontsize=20)
plt.savefig("ch05-firstorder-pole.pdf", bbox_inches='tight'); plt.close(fig)


## First order pole with 1/\tau=omegan label
fig, ax = create_complex_plane_axes(xrange=(-2.1, 1.5), yrange=(-1.5, 1.5))
draw_poles(ax, poles_to_plot, show_ticks=False)
ax.text(-1.1, -.5, r'$-{1}/{\tau}=-\omega_{\mathrm{n}}$', va='center', ha='center', fontsize=12)
plt.savefig("ch05-poles-firstorder.pdf", bbox_inches='tight'); plt.close(fig)


############# Second-order underdamped poles
zeta = 0.9
wn = 1.5
wd = wn * np.sqrt(1 - zeta**2) 
real_part = -zeta * wn        
poles_cc = [(real_part,  wd), (real_part, -wd)]
# Create plot with custom axis ranges (slightly taller for visibility)
fig, ax = create_complex_plane_axes(xrange=(-2.1, 1.5), yrange=(-1.5, 1.5))
draw_poles(ax, poles_cc, show_ticks=False)
# Annotations in the same spirit as your first-order plot
ax.text(real_part+0.1, -0.1, r"$-\zeta\omega_{\mathrm{n}}$", va="top", ha="left", fontsize=12)
ax.text(0.1, wd, r"$+\mathrm{i}\omega_{\mathrm{d}}$", va="center", ha="left", fontsize=12)
ax.text(0.1, -wd, r"$-\mathrm{i}\omega_{\mathrm{d}}$", va="center", ha="left", fontsize=12)
# Dashed vertical line connecting the two poles
ax.plot([real_part, real_part], [-wd, wd], linestyle="--", color="black", lw=0.5)
# Horizontal dashed lines from imaginary axis to poles
ax.plot([0, real_part], [wd,  wd], linestyle="--", color="black", lw=0.5)
ax.plot([0, real_part], [-wd, -wd], linestyle="--", color="black", lw=0.5)
draw_left_semicircle(ax, wn)  # passes through -ζω_n ± i ω_d (radius = ω_n)
plt.savefig("ch05-poles-secondorder-zeta-0p90.pdf", bbox_inches="tight"); plt.close(fig)


############# Second-order underdamped poles
zeta = 0.5
wn = 1.5
wd = wn * np.sqrt(1 - zeta**2) 
real_part = -zeta * wn        
poles_cc = [(real_part,  wd), (real_part, -wd)]
# Create plot with custom axis ranges (slightly taller for visibility)
fig, ax = create_complex_plane_axes(xrange=(-2.1, 1.5), yrange=(-1.5, 1.5))
draw_poles(ax, poles_cc, show_ticks=False)
# Annotations in the same spirit as your first-order plot
ax.text(real_part+0.1, -0.1, r"$-\zeta\omega_{\mathrm{n}}$", va="top", ha="left", fontsize=12)
ax.text(0.1, wd, r"$+\mathrm{i}\omega_{\mathrm{d}}$", va="center", ha="left", fontsize=12)
ax.text(0.1, -wd, r"$-\mathrm{i}\omega_{\mathrm{d}}$", va="center", ha="left", fontsize=12)
# Dashed vertical line connecting the two poles
ax.plot([real_part, real_part], [-wd, wd], linestyle="--", color="black", lw=0.5)
# Horizontal dashed lines from imaginary axis to poles
ax.plot([0, real_part], [wd,  wd], linestyle="--", color="black", lw=0.5)
ax.plot([0, real_part], [-wd, -wd], linestyle="--", color="black", lw=0.5)
draw_left_semicircle(ax, wn)  # passes through -ζω_n ± i ω_d (radius = ω_n)
plt.savefig("ch05-poles-secondorder-zeta-0p50.pdf", bbox_inches="tight"); plt.close(fig)

  
############# Three poles: at -ω_n and (-1/2 ± √3/2 i) ω_n
wn = 1.5; zeta = 0.5; wd = wn * np.sqrt(1 - zeta**2) 

poles_three = [ (-wn, 0.0), (-0.5 * wn,  (np.sqrt(3)/2) * wn), (-0.5 * wn, -(np.sqrt(3)/2) * wn) ]
# Create plot
fig, ax = create_complex_plane_axes(xrange=(-2.1, 1.5), yrange=(-1.5, 1.5))
draw_poles(ax, poles_three, show_ticks=False)

# Labels
ax.text(-wn+0.1, -0.15, r"$-\omega_{\mathrm{n}}$", va="top", ha="right", fontsize=12)
ax.text(real_part+0.05, -0.1, r"$-\tfrac{\omega_{\mathrm{n}}}{2}$", va="top", ha="left", fontsize=12)

ax.text(0.1, wd-0.1, r"$+\tfrac{\sqrt{3}\mathrm{i}}{2}\omega_{\mathrm{n}}$", va="center", ha="left", fontsize=12)
ax.text(0.1, -wd+0.1, r"$-\tfrac{\sqrt{3}\mathrm{i}}{2}\omega_{\mathrm{n}}$", va="center", ha="left", fontsize=12)

# ax.text(-0.5*wn + 0.1,  (np.sqrt(3)/2)*wn,
#         r"$\bigl(-\tfrac{1}{2}+\tfrac{\sqrt{3}}{2}\mathrm{i}\bigr)\omega_{\mathrm{n}}$", va="bottom", ha="left", fontsize=11)
# ax.text(-0.5*wn + 0.1, -(np.sqrt(3)/2)*wn,
#         r"$\bigl(-\tfrac{1}{2}-\tfrac{\sqrt{3}}{2}\mathrm{i}\bigr)\omega_{\mathrm{n}}$", va="top", ha="left", fontsize=11)

# Optional construction lines: # vertical dashed line through the complex pair
ax.plot([-0.5*wn, -0.5*wn],        [-(np.sqrt(3)/2)*wn, (np.sqrt(3)/2)*wn],        linestyle="--", color="black", lw=0.5)

# horizontal dashed lines from imag axis to the two conjugates
ax.plot([0, -0.5*wn], [(np.sqrt(3)/2)*wn, (np.sqrt(3)/2)*wn],  linestyle="--", color="black", lw=0.5)
ax.plot([0, -0.5*wn], [-(np.sqrt(3)/2)*wn, -(np.sqrt(3)/2)*wn],  linestyle="--", color="black", lw=0.5)
draw_left_semicircle(ax, wn)  
plt.savefig("ch05-poles-thirdorder.pdf", bbox_inches="tight"); plt.close(fig)
