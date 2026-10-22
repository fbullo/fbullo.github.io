# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

# root_locus_gallery_palettes.py
import numpy as np
import matplotlib.pyplot as plt
import control as ctrl

# --- Typography (toggle LaTeX off if your setup balks) ---
USE_LATEX = True
plt.rcParams.update({
    "text.usetex": USE_LATEX,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"],
    "axes.grid": False
})

# Define the requested colors from darker to lighter shades
blues = ['#002447', '#003c76', '#0055A4', '#006CD4', '#0085ff', '#239cff', '#58b1ff']
oranges = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']


# Helpers
def create_complex_plane_axes(xrange=(-3, 1), yrange=(-2, 2)):
    """Create and configure the complex plane axes with custom limits"""
    fig, ax = plt.subplots(figsize=(2, 2))
    
    # Axes lines
    ax.axhline(0, color='black', lw=.5)
    ax.axvline(0, color='black', lw=.5)
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

def draw_poles_zeros(ax, G, show_ticks=False):
    # poles = 'x', zeros = 'o'
    poles = ctrl.poles(G)
    zeros = ctrl.zeros(G)
    for p in poles:
        ax.plot(np.real(p), np.imag(p), 'x', color='#0055A4', ms=8, mew=1.5)
    for z in zeros:
        ax.plot(np.real(z), np.imag(z), 'o', mfc='none', mec='#a43e00', mew=1.5, ms=8)
    if show_ticks:
        # minimal reference lines at distinct pole/zero real parts
        xs = sorted({float(np.real(v)) for v in np.concatenate((poles, zeros))}) if zeros.size else \
             sorted({float(np.real(v)) for v in poles})
        for xr in xs: ax.axvline(xr, color='black', ls='--', lw=0.5)





def _split_branches(rlist):
    """Return a list of 1D complex arrays, one per branch, from ctrl.rlocus output."""
    r = np.asarray(rlist)
    if r.ndim == 1:                 # (Nk,)
        return [r]
    if r.shape[0] <= r.shape[1]:    # (nbranches, Nk)
        return [r[i, :] for i in range(r.shape[0])]
    else:                           # (Nk, nbranches)
        return [r[:, i] for i in range(r.shape[1])]

def plot_rlocus_custom(G, filename, Gtext,
                       xrange=(-5.5, 2.5), yrange=(-4, 4),
                       color='#006CD4', show_ticks=False):
    """
    Draw the root locus of G on your custom axes and save to filename.

    Args:
        G: control.TransferFunction
        filename: output PDF/PNG path
        Gtext: LaTeX fragment for the TF (e.g., r"\\dfrac{1}{(s+1)(s+2)}")
        xrange, yrange: axis limits (tuples)
        color: curve color (string from your palette or hex)
        show_ticks: if True, adds dashed ref lines at pole/zero real parts
    """
    # Axes
    fig, ax = create_complex_plane_axes(xrange=xrange, yrange=yrange)
    ax.set_prop_cycle(color=[color])

    # Root-locus data (no direct plotting)
    rlist, _ = ctrl.rlocus(G, plot=False, grid=False)

    # Plot each branch
    for br in _split_branches(rlist):
        ax.plot(np.real(br), np.imag(br), lw=1.5)

    # Poles and zeros
    draw_poles_zeros(ax, G, show_ticks=show_ticks)

    # Top-left label
    ax.text(0.02, 0.98, Gtext, transform=ax.transAxes, ha="left", va="top", fontsize=12)

    fig.tight_layout()
    fig.savefig(filename, bbox_inches="tight")
    plt.close(fig)




    
## setup s variable:
s = ctrl.TransferFunction.s



# 1) First order
plot_rlocus_custom(
    G=1/(s+2),
    filename="ch08-rootlocus-firstorder.pdf",
    Gtext=r"$\frac{1}{s+2}$",
    xrange=(-5.5, 2.5), yrange=(-4, 4),
    color=blues[3],
)

# 2) Second order (two real poles)
plot_rlocus_custom(
    G=1/((s+.9)*(s+2.7)),
    filename="ch08-rootlocus-secondorder.pdf",
    Gtext=r"$\frac{1}{(s+1)(s+3)}$",
    xrange=(-5.5, 2.5), yrange=(-4, 4),
    color=blues[3],
)

# 2cc) Second order (two real poles)
plot_rlocus_custom(
    G=1/(s**2+2*s+2),
    filename="ch08-rootlocus-secondorder-cc.pdf",
    Gtext=r"$\frac{1}{s^2+2s+2}$",
    xrange=(-3.5, 1.5), yrange=(-2.5, 2.5),
    color=blues[3],
)

# 3) Third order (three real poles)
plot_rlocus_custom(
    G=1/((s+1)*(s+2)*(s+3)),
    filename="ch08-rootlocus-thirdorder.pdf",
    Gtext=r"$\frac{1}{(s+1)(s+2)(s+3)}$",
    xrange=(-5.5, 2.5), yrange=(-4, 4),
    color=blues[3],
)

# 4a) Second order + zero
plot_rlocus_custom(
    G=(s+3)/((s+1)*(s+2)),
    filename="ch08-rootlocus-secondorder-zero-a.pdf",
    Gtext=r"$\frac{s+3}{(s+1)(s+2)}$",
    xrange=(-5.5, 2.5), yrange=(-4, 4),
    color=blues[3],
    show_ticks=False  # set True if you want dashed ref lines
)
# 4b) Second order + zero
plot_rlocus_custom(
    G=(s+2)/((s+1)*(s+3)),
    filename="ch08-rootlocus-secondorder-zero-b.pdf",
    Gtext=r"$\frac{s+2}{(s+1)(s+3)}$",
    xrange=(-5.5, 2.5), yrange=(-4, 4),
    color=blues[3],
    show_ticks=False  # set True if you want dashed ref lines
)
# 4c) Second order + zero
plot_rlocus_custom(
    G=(s+1)/((s+2)*(s+3)),
    filename="ch08-rootlocus-secondorder-zero-c.pdf",
    Gtext=r"$\frac{s+1}{(s+2)(s+3)}$",
    xrange=(-5.5, 2.5), yrange=(-4, 4),
    color=blues[3],
    show_ticks=False  # set True if you want dashed ref lines
)
# 4d) Second order + zero
plot_rlocus_custom(
    G=(s+2)/((s**2+2*s+2)),
    filename="ch08-rootlocus-secondorder-zero-d.pdf",
    Gtext=r"$\frac{s+2}{s^2+2s+2}$",
    xrange=(-5.5, 2.5), yrange=(-4, 4),
    color=blues[3],
    show_ticks=False  # set True if you want dashed ref lines
)


# 5) PI control of first-order systems
plot_rlocus_custom(
    G=(1/(s+2))*(1+3/s),
    filename="ch08-rootlocus-pi-firstorder.pdf",
    Gtext=r"$\frac{1}{s+2}(1+\frac{3}{s})$",
    xrange=(-5.5, 2.5), yrange=(-4, 4),
    color=blues[3],
    show_ticks=False  # set True if you want dashed ref lines
)



# unmodeled dynamics and destabilizing control design
plot_rlocus_custom(
    G=(s+8)/(s*(s+1)*(s+5)),
    filename="ch08-rootlocus-random-to-erase.pdf",
    Gtext=r"$\frac{s+8}{s(s+1)(s+5)}$",
    xrange=(-10, 2.5), yrange=(-8, 8),
    color=blues[3],
)
