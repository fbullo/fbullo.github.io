(*
 * DerivativesOptions.m
 *
 * Andrew D. Lewis
 * Queen's University
 * 24 August 1999
 *
 * Provides the types of differentiation to vaious packages requiring
 * such.
 *
 *)

BeginPackage["DerivativeOptions`"]

Deriv::usage = "Deriv is an option for function calls in Affine.m which
involve differentiation.  Deriv->Explicit for explicit differentiation of
functions, and Deriv->Implicit does implicit differentiation.";

Explicit::usage = "Option for Deriv[].  Does explicit differentiation of
functions.  Default.";

Implicit::usage = "Option for Deriv[].  Does implicit differentiation of
functions.";

EndPackage[]
