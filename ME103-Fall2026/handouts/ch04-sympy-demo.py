# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

# Import SymPy for symbolic math operations
from sympy import symbols, diff, integrate, Eq, solve, sin, cos, series

# Define a symbol x
x = symbols('x')

# 1. Differentiate a symbolic expression
expr = sin(x) * cos(x)
derivative = diff(expr, x)
print("Example calculations performed by SymPy:\n")
print(f"Derivative of sin(x) * cos(x): {derivative}")

# 2. Integrate a symbolic expression
integral = integrate(expr, x)
print(f"Integral of sin(x) * cos(x): {integral}")

# 3. Solve a symbolic equation
equation = Eq(x**2 + 2*x - 8, 0)
solutions = solve(equation, x)
print(f"Solutions to x^2 + 2x - 8 = 0: {solutions}")

# 4. Perform a Taylor series expansion
taylor_series = series(sin(x), x, 0, 6)
print(f"Taylor series of sin(x): {taylor_series}")

