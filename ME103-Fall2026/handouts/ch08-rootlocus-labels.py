# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
import control as ctrl

# ---------------- Typography ----------------
USE_LATEX = True
plt.rcParams.update({
    "text.usetex": USE_LATEX,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"],
    "axes.grid": False
})

# Palettes
blues = ['#002447', '#003c76', '#0055A4', '#006CD4', '#0085ff', '#239cff', '#58b1ff']
oranges = ['#471b00', '#752d00', '#a43e00', '#d35000', '#ff6100', '#ff7f1a', '#ff9b56']

# ---------------- Helpers ----------------
def create_complex_plane_axes(xrange, yrange, figsize):
    fig, ax = plt.subplots(figsize=figsize)
    ax.axhline(0, color='black', lw=.5)
    ax.axvline(0, color='black', lw=.5)
    ax.set_xlim(xrange); ax.set_ylim(yrange)
    ax.set_box_aspect(figsize[1] / figsize[0])
    ax.plot(1, 0, ">k", transform=ax.get_yaxis_transform(), clip_on=False)
    ax.plot(0, 1, "^k", transform=ax.get_xaxis_transform(), clip_on=False)
    ax.set_xticks([]); ax.set_yticks([])
    ax.tick_params(axis='x', which='both', length=0)
    ax.tick_params(axis='y', which='both', length=0)
    for spine in ax.spines.values(): spine.set_visible(False)
    return fig, ax

def _split_branches(rlist):
    r = np.asarray(rlist)
    if r.ndim == 1: return [r]
    if r.shape[0] <= r.shape[1]: return [r[i, :] for i in range(r.shape[0])]
    return [r[:, i] for i in range(r.shape[1])]


def _fmt_complex(z, dec=2):
    """Format a complex number as a LaTeX math string, suppressing '1' before i."""
    x, y = float(np.real(z)), float(np.imag(z))
    fmt = f".{dec}f"
    # Pure real?
    if abs(y) < 10**(-dec-1):
        return fr"${x:{fmt}}$"
    sign = "+" if y >= 0 else "-"
    ay = abs(y)
    # Suppress coefficient if ~1.0 within rounding
    if round(ay, dec) == 1.0:
        return fr"${x:{fmt}} {sign} \mathrm{{i}}$"
    else:
        return fr"${x:{fmt}} {sign} {ay:{fmt}} \mathrm{{i}}$"

    
def draw_poles_zeros(ax, G, pole_dec=0, zero_dec=3, fontsize=12, offset_points=(0, -6)):
    poles = np.atleast_1d(ctrl.poles(G))
    zeros = np.atleast_1d(ctrl.zeros(G))
    for p in poles:
        x, y = float(np.real(p)), float(np.imag(p))
        ax.plot(x, y, 'x', color=blues[2], ms=8, mew=1.5)
        ax.annotate(_fmt_complex(p, pole_dec), xy=(x, y), xytext=offset_points,
                    textcoords='offset points', ha='center', va='top', fontsize=fontsize)
    for z in zeros:
        x, y = float(np.real(z)), float(np.imag(z))
        ax.plot(x, y, 'o', mfc='none', mec=oranges[2], mew=1.5, ms=8)
        ax.annotate(_fmt_complex(z, zero_dec), xy=(x, y), xytext=offset_points,
                    textcoords='offset points', ha='center', va='top', fontsize=fontsize)

def _normalize_kmarks(Kmarks, default_dec, default_fontsize):
    for km in Kmarks:
        if isinstance(km, dict):
            k_raw = km.get("k", None)
            if k_raw == "inf":
                kval = np.inf
            else:
                kval = float("inf") if (k_raw is None and km.get("inf", False)) else (np.inf if k_raw is None and km.get("k") == np.inf else k_raw)
            if kval is None:
                raise ValueError("Each dict in Kmarks must include 'k' (or set 'inf': True).")
            if "left" in km:
                pos_kind = "left"
                above = tuple(km.get("left"))
                ha_label, va_label = "right", "center"
            elif "right" in km:
                pos_kind = "right"
                above = tuple(km.get("right"))
                ha_label, va_label = "left", "center"
            else:
                pos_kind = "above"
                above = tuple(km.get("above", (0, +6)))
                ha_label, va_label = "center", "bottom"
            spec = {
                "k": (np.inf if (isinstance(kval, str) and str(kval).lower() == "inf") else kval),
                "pos_kind": pos_kind,
                "above": above,
                "below": tuple(km.get("below", (0, -6))),
                "ha_label": km.get("ha_label", ha_label),
                "va_label": km.get("va_label", va_label),
                "dec": int(km.get("dec", default_dec)),
                "fontsize": int(km.get("fontsize", default_fontsize)),
                "rotation": float(km.get("rotation", 0.0)),
                "rotation_label": float(km.get("rotation_label", 0.0)),
                "label_roots": bool(km.get("label_roots", False)),
                "only_indices": km.get("only_indices", None),
                "label": km.get("label", None),
                "color": km.get("color", "black"),
            }
        else:
            spec = {
                "k": km,
                "pos_kind": "above",
                "above": (0, +6),
                "below": (0, -6),
                "ha_label": "center",
                "va_label": "bottom",
                "dec": default_dec,
                "fontsize": default_fontsize,
                "rotation": 0.0,
                "rotation_label": 0.0,
                "label_roots": False,
                "only_indices": None,
                "label": None,
                "color": "black",
            }
        yield spec

def plot_rlocus_with_gain_marks(
    G, filename, Gtext, Kmarks,
    xrange, yrange, figsize=(4, 3), color=blues[3], dec=1,
    tf_fontsize=24, ol_pole_dec=0, ol_zero_dec=None, ol_fontsize=None
):
    fig, ax = create_complex_plane_axes(xrange=xrange, yrange=yrange, figsize=figsize)
    ax.set_prop_cycle(color=[color])
    rlist, _ = ctrl.rlocus(G, plot=False, grid=False)
    for br in _split_branches(rlist):
        ax.plot(np.real(br), np.imag(br), lw=1.5)
    zero_dec_eff = dec if ol_zero_dec is None else ol_zero_dec
    draw_poles_zeros(
        ax, G,
        pole_dec=ol_pole_dec,
        zero_dec=zero_dec_eff,
        fontsize=(ol_fontsize if ol_fontsize is not None else 12),
        offset_points=(0, -6)
    )
    zeros = np.atleast_1d(ctrl.zeros(G))
    for spec in _normalize_kmarks(Kmarks, default_dec=dec, default_fontsize=12):
        k = spec["k"]
        above = spec["above"]; below = spec["below"]
        ha_lab = spec["ha_label"]; va_lab = spec["va_label"]
        kdec = spec["dec"]; kfz = spec["fontsize"]
        rot = spec["rotation"]; rot_label = spec["rotation_label"]
        label_roots = spec["label_roots"]; only_idx = spec["only_indices"]; klabel = spec["label"]
        kcolor = spec["color"]
        if k is np.inf or (isinstance(k, float) and np.isinf(k)):
            for zi, z in enumerate(zeros):
                if (only_idx is not None) and (zi not in only_idx):
                    continue
                xk, yk = float(np.real(z)), float(np.imag(z))
                ax.plot(xk, yk, 'o', mfc=oranges[4], mec='none', ms=5, zorder=6)
                ax.annotate(klabel if klabel is not None else r"$k=\infty$",
                            xy=(xk, yk), xytext=above, textcoords='offset points',
                            ha=ha_lab, va=va_lab, fontsize=kfz, rotation=rot_label)
            if len(ctrl.poles(G)) > len(zeros):
                ax.annotate(r"$\to -\infty$ (as $k\to\infty$)",
                            xy=(xrange[0], 0),
                            xytext=(xrange[0] + 0.12*(xrange[1]-xrange[0]), 0),
                            ha='left', va='center', fontsize=12,
                            arrowprops=dict(arrowstyle='-|>', lw=0.8))
            continue
        Gcl = ctrl.feedback(float(k) * G, 1)
        roots = np.atleast_1d(ctrl.poles(Gcl))
        for ri, rk in enumerate(roots):
            if (only_idx is not None) and (ri not in only_idx):
                continue
            xk, yk = float(np.real(rk)), float(np.imag(rk))
            ax.plot(xk, yk, 'o', mfc=oranges[4], mec='none', ms=5, zorder=6)
            ax.annotate(klabel if klabel is not None else fr"$k={k}$",
                        xy=(xk, yk), xytext=above, textcoords='offset points',
                        ha=ha_lab, va=va_lab, fontsize=kfz, rotation=rot_label, color=kcolor)
            if (k != 0) or label_roots:
                ax.annotate(_fmt_complex(rk, kdec), xy=(xk, yk), xytext=below,
                            textcoords='offset points', ha='center', va='top',
                            fontsize=kfz, rotation=rot, color=kcolor)
    ax.text(0.02, 0.98, Gtext, transform=ax.transAxes,
            ha="left", va="top", fontsize=tf_fontsize)
    fig.tight_layout()
    fig.savefig(filename, bbox_inches="tight")
    plt.close(fig)

# ---------------- Systems & Example Calls ----------------
s = ctrl.TransferFunction.s
FIGSIZE = (4, 3)


# (1) First order: per-K custom placement/orientation/accuracy
plot_rlocus_with_gain_marks(
    G=1/(s+2),
    filename="ch08-rootlocus-firstorder-labels.pdf",
    Gtext=r"$\frac{1}{s+2}$",
    Kmarks=[
        0,  # default placement/dec
        {"k": 1, "above": (0, 6),  "below": (0, -6), "dec": 0, "fontsize": 12, "rotation_label": 90},
        {"k": 2, "above": (0, 6),  "below": (0, -6), "dec": 0, "fontsize": 12, "rotation_label": 90},
        {"k": 3, "above": (0, 6),  "below": (0, -6), "dec": 0, "fontsize": 12, "rotation_label": 90},
        {"k": 4, "above": (0, 6),  "below": (0, -6), "dec": 0, "fontsize": 12, "rotation_label": 90},
    ],
    xrange=(-7.5, 1.5),
    yrange=(-1.2, 1.2),
    figsize=FIGSIZE,
    color=blues[3],
    dec=0,
    tf_fontsize=24
)


# (2) Second order: 1/((s+1)(s+3)) with per-K tweaks (using left/right)
plot_rlocus_with_gain_marks(
    G=1/((s+1)*(s+3)),
    filename="ch08-rootlocus-secondorder-labels.pdf",
    Gtext=r"$\frac{1}{(s+1)(s+3)}$",
    Kmarks=[
        0,
        {"k": 1, "right": (4, 8), "below": (0, -6), "dec": 0},  # K label to the right
        {"k": 2, "right":  (4, 0), "below": (0, -6), "dec": 0},  # K label to the left
    ],
    xrange=(-5, 1),
    yrange=(-1.5, 1.5),
    figsize=FIGSIZE,
    color=blues[3],
    dec=0,
    tf_fontsize=24
)

# (3) Second order + zero: (s+1)/(s(s+2)) with K=0, 1.5, ∞, each customized
plot_rlocus_with_gain_marks(
    G=(s+1)/(s*(s+2)),   # the system of the caption of Figure 8.10 (fixed 2026-09-24)
    filename="ch08-rootlocus-secondorder-zero-labels.pdf",
    Gtext=r"$\frac{s+1}{s(s+2)}$",
    Kmarks=[
        0,
        {"k": 1.5, "above": (0, 6), "below": (0, -6), "dec": 1, "rotation_label": 90, "rotation": 90}
##        , {"k": "inf", "right": (8, 0), "fontsize": 12},  # mark finite zero with K=∞ label to the right
    ],
    xrange=(-5.0, 1),
    yrange=(-2, 2),
    figsize=FIGSIZE,
    color=blues[3],
    dec=1,
    tf_fontsize=24
)

# (4) Third order: 1/(s(s+3)(s+6)), the system of the caption of Figure 8.11
#     (fixed 2026-09-24); k=162 is the imaginary-axis crossing, s=+-i sqrt(18)
G4 = 1/(s*(s+3)*(s+6))
r162 = np.atleast_1d(ctrl.poles(ctrl.feedback(162*G4, 1)))  # -9 and +-i sqrt(18)
i_real = [i for i, r in enumerate(r162) if abs(r.imag) < 1e-6]   # k label above the pole -9
i_cplx = [i for i, r in enumerate(r162) if abs(r.imag) >= 1e-6]  # k labels right of +-i sqrt(18)
plot_rlocus_with_gain_marks(
    G=G4,
    filename="ch08-rootlocus-thirdorder-labels.pdf",
    Gtext=r"$\frac{1}{s(s+3)(s+6)}$",
    Kmarks=[
        0,
        {"k": 162, "right": (6, 0), "below": (0, -6), "dec": 2, "label": r"$k\!=\!162$", "color": "#d35000", "only_indices": i_cplx},  # gnbrown4: new 2026-09-24
        {"k": 162, "above": (0, 6), "below": (0, -6), "dec": 2, "label": r"$k\!=\!162$", "color": "#d35000", "only_indices": i_real},
    ],
    xrange=(-10.5, 2.0),
    yrange=(-6.0, 6.0),
    figsize=FIGSIZE,
    color=blues[3],
    dec=1,
    tf_fontsize=24
)
