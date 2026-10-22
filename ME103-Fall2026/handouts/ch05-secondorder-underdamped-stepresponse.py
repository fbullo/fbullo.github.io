# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
plt.rcParams.update({"text.usetex": True, "font.family": "serif", "font.serif": ["Computer Modern Roman"] })

# Define the requested colors from darker to lighter shades
blues = ['#002447', '#003c76', '#0055A4', '#006CD4', '#0085ff', '#239cff', '#58b1ff']
oranges = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']

def plot_second_order(zeta, filename):
    wn = 1.0
    wd = wn * np.sqrt(1 - zeta**2)
    phi = np.arctan(np.sqrt(1 - zeta**2) / zeta)

    # Metrics
    T_rise_0_100 = (np.pi - phi) / wd          # first time y(t) reaches 1
    T_peak = np.pi / wd                        # first peak
    M_percent = np.exp(-zeta * np.pi / np.sqrt(1 - zeta**2)) * 100.0
    T_settling_5 = 3.0 / (zeta * wn)           # ~5% settling time approximation

    # Time grid
    t = np.linspace(0, 12, 2000)

    # Step response of 2nd-order underdamped system with unit DC gain
    A = 1 / np.sqrt(1 - zeta**2)
    y = 1 - A * np.exp(-zeta * wn * t) * np.sin(wd * t + phi)

    # Plot
    ## fig, ax = plt.subplots(figsize=(7, 4))
    fig, ax = plt.subplots(figsize=(5.6, 3.2))
    line, = ax.plot(t, y, linewidth=2, color=blues[4], label="$y(t)$")
    ax.axhline(1.0, linestyle="--", linewidth=1, color="#666666")

    ax.set_xlim(0, 12)
    ax.set_xticks(np.arange(0, 13, 2))
    ax.set_xlabel("time $t$")

    ax.set_ylim(0, 1.4)
    ax.set_yticks([0, 0.5, 1.0, 1.25, 1.4])

    y_peak = 1 + M_percent / 100.0
    if zeta >= 0.7:
        rise_label_position = (T_rise_0_100, 0.94, "left", "top")
        peak_label_position = (T_peak, y_peak + 0.12, "center", "bottom")
        settling_label_position = (T_settling_5, 1.08, "right", "bottom")
    else:
        rise_label_position = (T_rise_0_100, 0.98, "left", "top")
        peak_label_position = (T_peak, y_peak + 0.02, "left", "bottom")
        settling_label_position = (T_settling_5, 1.06, "left", "bottom")

    # ----- Trise -----
    # Vertical marker for T_{rise,0%-100%} (stop at y=1.0)
    trise_vline, = ax.plot([T_rise_0_100, T_rise_0_100], [0, 1.0], linestyle=":", linewidth=1, color=oranges[0],
                           label=f"$T_{{\\mathrm{{rise,0\\%-100\\%}}}} = {T_rise_0_100:0.2f}$")
    # Dot at (T_{rise,0%-100%}, 1.0)
    ax.plot(T_rise_0_100, 1.0, 'o', color=oranges[0], markersize=3)
    # Label near the point
    ax.text(
        rise_label_position[0],
        rise_label_position[1],
        "$T_{\\mathrm{rise,0\\%-100\\%}}$",
        ha=rise_label_position[2],
        va=rise_label_position[3],
        color=oranges[0],
    )

    # ----- Tpeak -----
    # Vertical marker for Tpeak (stop at overshoot value)
    tpeak_vline, = ax.plot([T_peak, T_peak], [0, y_peak], linestyle=":", linewidth=1, color=oranges[2],
                            label=f"$T_{{\\mathrm{{peak}}}} = {T_peak:0.2f}$")
    # Dot at (Tpeak, y_peak)
    ax.plot(T_peak, y_peak, 'o', color=oranges[2], markersize=3)
    # Label near the point
    ax.text(
        peak_label_position[0],
        peak_label_position[1],
        "$T_{\\mathrm{peak}}$",
        ha=peak_label_position[2],
        va=peak_label_position[3],
        color=oranges[2],
    )
    # Horizontal segment from overshoot point back to vertical axis
    ax.plot([0, T_peak], [y_peak, y_peak], linestyle="--", linewidth=1, color=oranges[2])

    # ----- Tsettling -----
    # Vertical marker for T_{settling 5%} (stop at y=1.0)
    tsettle_vline, = ax.plot([T_settling_5, T_settling_5], [0, 1.0], linestyle=":", linewidth=1, color=oranges[4],
                              label=f"$T_{{\\mathrm{{settling\\ 5\\%}}}} = {T_settling_5:0.2f}$")
    # Dot at (T_{settling 5%}, 1.0)
    ax.plot(T_settling_5, 1.0, 'o', color=oranges[4], markersize=3)
    # Label near the point
    ax.text(
        settling_label_position[0],
        settling_label_position[1],
        "$T_{\\mathrm{settling\\ 5\\%}}$",
        ha=settling_label_position[2],
        va=settling_label_position[3],
        color=oranges[4],
    )

    # Horizontal settling band lines at 0.95 and 1.05 (to the right of T_{settling 5%})
    ax.hlines([0.95, 1.05], xmin=T_settling_5, xmax=ax.get_xlim()[1], linestyle="--", linewidth=1, color=oranges[4])
    # Shaded settling band region (to the right of T_{settling 5%})
    ax.fill_betweenx([0.95, 1.05], T_settling_5, ax.get_xlim()[1], color=oranges[4], alpha=0.1)

    # ----- Legend-only entry for overshoot -----
    dummy_line, = ax.plot([], [], alpha=0, label=f"$M_{{\\mathrm{{percent}}}} = {M_percent:0.1f}\\%$")

    # ----- Legend (ordered) -----
    handles = [line, trise_vline, tpeak_vline, tsettle_vline, dummy_line]
    labels = [h.get_label() for h in handles]

    leg = ax.legend(
        handles,
        labels,
        loc="lower right",
        fontsize=10,
        frameon=True,
        facecolor="white",        # background
        edgecolor="#f5f5f5",      # border
        title=rf"$\omega_{{\mathrm{{n}}}}=1,\ \zeta={zeta:.2f}$",
        title_fontsize=11,
        alignment="center"        # center title + entries (Matplotlib ≥ 3.8)
    )

    plt.tight_layout()
    plt.savefig(filename, format="pdf", bbox_inches="tight")
    plt.close(fig)

# ---- Generate both figures ----
plot_second_order(0.40, "ch05-secondorder-underdamped-stepresponse-40.pdf")
plot_second_order(0.80, "ch05-secondorder-underdamped-stepresponse-80.pdf")
