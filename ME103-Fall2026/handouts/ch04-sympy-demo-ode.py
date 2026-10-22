# Francesco Bullo, ME 103 "Dynamical Systems", Fall 2026,
# Mechanical Engineering, UC Santa Barbara.
# Copyright (c) 2023-2026 Francesco Bullo.  MIT License:
# https://opensource.org/license/mit

# Python code to solve a differential equation
from sympy import Function, dsolve, Eq, symbols, init_printing
from sympy.abc import t

# Define the symbols
y0, v0 = symbols('y0 v0')

# Define the function which represents y(t)
y = Function('y')

# Define the differential equation as in Section 4.3
diffeq = Eq(y(t).diff(t, t) + 7*y(t).diff(t) + 12*y(t), 0)

# Solve the differential equation with initial conditions
solution = dsolve(diffeq, y(t), ics={y(0): y0, y(t).diff(t).subs(t, 0): v0})

# Extract and print the right-hand side
print("The ode solution is:")
rhs = solution.rhs; print("y(t) =", rhs)

