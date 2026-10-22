# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt

blues = ['#002447', '#003c76', '#0055A4', '#006CD4', '#0085ff', '#239cff', '#58b1ff']
grays = [
    '#1a1a1a',  # Very dark gray (almost black)
    '#333333',  # Dark gray
    '#4d4d4d',  # Medium-dark gray
    '#666666',  # Medium gray
    '#999999',  # Light-medium gray
    '#cccccc',  # Light gray
    '#e6e6e6',  # Very light gray
    '#f5f5f5',  # Whitesmoke (ultra light gray)
]

# Define time array
t = np.linspace(0, 4 * np.pi, 500)

# Define functions and their titles as (title, y) pairs
functions = {
    'unit_step':        ('unit step',            np.ones_like(t)),
    't':                ('ramp',                 t),
    'tsquare':          ('quadratic growth',     t**2),
    'exp_t':            ('exponential growth',   np.exp(t/3)),
    'exp_2t':           ('faster growth',        np.exp(2*t/3)),
    'exp_neg_t':        ('exponential decay',    np.exp(-t/3)),
    'sin_t':            ('sinusoidal wave',      np.sin(2*t)),
    'cos_t':            ('cosine wave',          np.cos(2*t)),
    'sin_2t':           ('high-frequency sine',  np.sin(4*t)),
    'exp_sin_t':        ('growing sine wave',    np.exp(t/2.5) * np.sin(2*t)),
    'exp_neg_sin_t':    ('decaying sine wave',     np.exp(-t/2.5) * np.sin(2*t)),
}

def plot_with_custom_axes(t, y, title, filename, xrange=None, yrange=None):
    """
    Plots a function with centered axes that terminate in arrowheads at the
    edge of the plot frame.
    """
    fig, ax = plt.subplots(figsize=(5, 4))
    ax.plot(t, y, color=blues[2])

    fig.patch.set_facecolor(grays[6])  # Background outside the axes
    ax.set_facecolor(grays[6])         # Background inside the axes

    # Set default range if not specified
    if xrange is None:
        xrange = (np.min(t) - 0.2, np.max(t))

    # Calculate y-range based on data, but force it to include y=0
    if yrange is None:
        ypad = 0.1 * (np.max(y) - np.min(y))
        if ypad == 0:  # Handle flat lines
            ypad = 0.3
        data_min_y = np.min(y) - ypad
        data_max_y = np.max(y) + ypad
        yrange = (min(data_min_y, 0), max(data_max_y, 0))

    # Set plot limits
    ax.set_xlim(xrange)
    ax.set_ylim(yrange)

    # Draw centered axes lines that span the entire plot
    ax.axhline(0, color='black', lw=1)
    ax.axvline(0, color='black', lw=1)

    # Arrowheads at ends of the axes
    ax.plot(1, 0, ">k", transform=ax.get_yaxis_transform(), clip_on=False)
    ax.plot(0, 1, "^k", transform=ax.get_xaxis_transform(), clip_on=False)

    # Title inside the plot frame
    ax.set_title(
        title,
        y=0.95,
        pad=-10,
        fontsize=20,
        color='black'
    )

    # Remove ticks and spines
    ax.set_xticks([])
    ax.set_yticks([])
    for spine in ax.spines.values():
        spine.set_visible(False)

    plt.tight_layout(pad=0.5)
    plt.savefig(f"{filename}.pdf")
    # plt.show()

# Generate and save each plot
for fname, (title, y) in functions.items():
    if fname == 'unit_step':
        plot_with_custom_axes(t, y, title=title, filename=f'ch04-plot-{fname.replace("_","-")}', yrange=(-0.2, 2))
    else:
        plot_with_custom_axes(t, y, title=title, filename=f'ch04-plot-{fname.replace("_","-")}')


        
