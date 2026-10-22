# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Arc


plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.serif": ["Computer Modern Roman"],
})

BLUE = "#0055A4"
ORANGE = "#d35000"
GRAY = "#666666"
FIGURE_SIZE = (3.6, 3.6)


def complex_plane(axis, x_limits, y_limits=None):
    axis.axhline(0, color=GRAY, linewidth=0.8)
    axis.axvline(0, color=GRAY, linewidth=0.8)
    axis.set_aspect("equal", adjustable="box")
    if y_limits is None:
        y_limits = x_limits
    axis.set_xlim(*x_limits)
    axis.set_ylim(*y_limits)
    axis.set_xticks([])
    axis.set_yticks([])
    axis.spines[["top", "right", "left", "bottom"]].set_visible(False)


def phasor(axis, length, angle, color, label=None, label_offset=1.08,
           linestyle="-", linewidth=2.8):
    endpoint = length * np.array([np.cos(angle), np.sin(angle)])
    axis.annotate(
        "",
        xy=endpoint,
        xytext=(0, 0),
        arrowprops={
            "arrowstyle": "-|>",
            "color": color,
            "linewidth": linewidth,
            "linestyle": linestyle,
            "mutation_scale": 16,
        },
    )
    if label:
        label_position = label_offset * endpoint
        axis.text(
            label_position[0], label_position[1], label,
            color=color, fontsize=15, ha="center", va="center",
        )
    return endpoint


def save_definition():
    figure, axis = plt.subplots(figsize=FIGURE_SIZE)
    complex_plane(axis, (-.16, 1.14), (-.16, 1.14))
    angle = np.pi / 4
    phasor(axis, 1.0, angle, BLUE, r"$a\mathrm{e}^{\mathrm{i}\theta}$")
    axis.add_patch(Arc((0, 0), 0.42, 0.42, angle=0, theta1=0,
                       theta2=np.degrees(angle), color=ORANGE, linewidth=2.0))
    axis.text(0.28, 0.10, r"$\theta$", color=ORANGE, fontsize=15)
    figure.subplots_adjust(left=.02, right=.98, bottom=.02, top=.98)
    figure.savefig("ch06-phasors-def.pdf")
    plt.close(figure)


def save_equal_frequency_sum():
    figure, axis = plt.subplots(figsize=FIGURE_SIZE)
    complex_plane(axis, (-.62, 1.35), (-.16, 1.35))
    first = phasor(axis, 0.95, np.radians(25), BLUE,
                  r"$a\mathrm{e}^{\mathrm{i}\theta}$")
    second = phasor(axis, 0.90, np.radians(120), ORANGE,
                   r"$b\mathrm{e}^{\mathrm{i}\phi}$")
    result = first + second
    axis.plot(
        [first[0], result[0]], [first[1], result[1]],
        color=GRAY, linestyle="--", linewidth=1,
    )
    axis.plot(
        [second[0], result[0]], [second[1], result[1]],
        color=GRAY, linestyle="--", linewidth=1,
    )
    phasor(axis, np.linalg.norm(result), np.arctan2(result[1], result[0]),
           "#333333", r"$a+b$", label_offset=1.10)
    figure.subplots_adjust(left=.02, right=.98, bottom=.02, top=.98)
    figure.savefig("ch06-phasors-sum.pdf")
    plt.close(figure)


def save_beating(name, second_angle, second_length):
    figure, axis = plt.subplots(figsize=FIGURE_SIZE)
    first_vector = np.array([np.cos(np.radians(12)), np.sin(np.radians(12))])
    second_vector = second_length * np.array(
        [np.cos(second_angle), np.sin(second_angle)]
    )
    result = first_vector + second_vector
    if result[0] > 1.5:
        complex_plane(axis, (-.12, 2.20), (-.12, 1.05))
    else:
        complex_plane(axis, (-1.08, 1.32), (-.42, .42))
    first = phasor(axis, 1.0, np.radians(12), BLUE, r"$a$", label_offset=1.12)
    second = phasor(
        axis, second_length, second_angle, ORANGE, r"$b$", label_offset=1.12
    )
    result = first + second
    result_length = np.linalg.norm(result)
    if result_length > 1e-3:
        phasor(axis, result_length, np.arctan2(result[1], result[0]),
               "#333333", None, label_offset=1.18, linewidth=1.8)
        if result_length < 0.4:
            axis.text(0.28, 0.24, r"$a+b$", color="#333333", fontsize=15)
        else:
            label_offset = 1.08 if result[0] > 1.5 else 1.18
            label_alignment = "right" if result[0] > 1.5 else "center"
            label_position = label_offset * result
            axis.text(label_position[0], label_position[1], r"$a+b$",
                      color="#333333", fontsize=15, ha=label_alignment)
    figure.subplots_adjust(left=.02, right=.98, bottom=.02, top=.98)
    figure.savefig(f"{name}.pdf")
    plt.close(figure)


save_definition()
save_equal_frequency_sum()
save_beating("ch06-phasors-beats1", np.radians(25), 1.0)
save_beating("ch06-phasors-beats2", np.radians(190), 0.82)
