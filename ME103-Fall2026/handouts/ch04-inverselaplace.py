# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

from sympy import symbols, inverse_laplace_transform, latex
from sympy.abc import s, t

# Define symbols
a, b, c, omega = symbols('a b c omega', real=True)

# Define the rational functions for which we want the inverse Laplace transform
functions = [
    2 + 3/s + 4/(s + 5),           # ex1: a single pole
    (s + 2) / (s**2 + 7*s + 12),   # ex2: two real poles
    2 / (s**3 + 6*s**2 + 11*s + 6),# ex3: multiple isolated poles
    (8*s + 12) / (s**2 + 6*s + 25),# ex4: complex conjugate poles    
    (s**2 + 3*s + 3) / (s + 2)**3, # ex5: a repeated pole
    # Symbolic examples
    (a*s + b) / (s**2 + 7*s + 12), # ex6: two poles
    1 / ((s + a) * (s + b) * (s + c)), # ex7: three poles
    1 / ((s + a) * (s**2 + omega**2))  # ex8: a real, two conjugate poles
]

# Prepare the LaTeX content
latex_content = "Examples of inverse Laplace transforms:\n\\begin{align}\n"

# Loop through the functions
for i, F in enumerate(functions):
    # Compute the inverse Laplace transform
    f = inverse_laplace_transform(F, s, t)

    # Generate the LaTeX line for the current function
    latex_line = f"\\mathcal{{L}}^{{-1}} \\left[ {latex(F)} \\right] &= {latex(f)} \\label{{eq:example{i+1}}}"

    # Add a line break after each equation, except the last one
    if i < len(functions) - 1:
        latex_line += " \\\\ \n"

    # Add vertical space after the first five examples
    if i == 4:
        latex_line += "\\nonumber\\\\ \n"

    # Add the current line to the LaTeX content
    latex_content += latex_line

# Close the LaTeX content with align
latex_content += "\\end{align}\n"

# Write the LaTeX to a file
with open("ch04-inverselaplace.tex", "w") as file:
    file.write(latex_content)



