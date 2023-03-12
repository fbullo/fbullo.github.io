(*
 * Affine.m
 *
 * Andrew D. Lewis
 * California Institute of Technology
 * 26 January 1996
 * Last revised: 25 August 1999
 *
 * Mathematica for affine differential geometry.
 *
 *)

BeginPackage["Affine`",{"Tensors`"}]

AlongCurve::usage = "AlongCurve[A,Conn,c,t,Type,Deriv] returns the covariant
derivative of the tensor field A of type Type along the curve c.  t is the
time parameter which c must depend upon.";

CovariantDerivative::usage = "CovariantDerivative[X,Y,Conn,x,Deriv]
computes the covariant derivative of Y along X with respect to a connection
Conn in coordinates x (nabla_XY).";

CovariantDifferential::usage = "CovariantDifferential[A,Conn,x,Type,Deriv]
returns the covariant differential of a tensor A of type Type in
coordinates x.";

CurvatureTensor::usage = "CurvatureTensor[Conn,x,Deriv] computes the
components of the curvature tensor for the connection Conn in coordinates
x.";

Grad::usage = "Grad[f,g,x,Deriv] computes the gradient of f with
respect to the metric g in coordinates x.";

LeviCivita::usage = "LeviCivita[g,x,Deriv] computes the Christoffel symbols of
the Levi-Civita connection with metric g with respect to the coordinates x.";

RicciCurvature::usage = "RicciCurvature[Conn,x,Deriv] computes the components
of the Ricci tensor for the affine connection with Christoffel symbols Conn
in coordinates x.";

RiemannFlat::usage = "RiemannFlat[X,g] computes the image of the vector field
X under the musical isomorphism associated with g.";

RiemannSharp::usage = "RiemannSharp[w,g] computes the image of the one-form w
under the musical isomorphism associated with g.";

ScalarCurvature::usage = "ScalarCurvature[g,x,Deriv] computes the scalar
curvature for the Levi-Civita connection for the metric g in coordinates x.";

SectionalCurvature::usage = "SectionalCurvature[X,Y,g,x,Deriv] computes the
sectional curvature of the plane spanned by the orthonormal vectors X and
Y.";

Spray::usage = "Spray[Conn,x,xdot] computes the geodesic spray associated to
the connection Conn in coordinates x with velocities xdot.";

SymmetricProduct::usage = "SymmetricProduct[X,Y,Conn,x,Deriv] computes the
symmetric product of X and Y relative to covariant differentiation with
respect to the connection Conn in coordinates x.";

TorsionTensor::usage = "TorsionTensor[Conn,x] provides the torsion tensor for
the affine connection with Christoffel symbols Conn.";

Begin["`Private`"]

Print["\nPackage \"Affine\" defines: AlongCurve, CovariantDerivative, CovariantDifferential, CurvatureTensor, Grad, LeviCivita, RicciCurvature, RiemannFlat, RiemannSharp, ScalarCurvature, SectionalCurvature, Spray, SymmetricProduct, TorsionTensor."];

(*** AlongCurve ***)

Options[AlongCurve] = {Deriv->Explicit}
AlongCurve[A_,Conn_,c_,t_,Type_,opts___Rule] :=
  Module[{opt1,local,DerivOpts},
  opt1 = Deriv /. {opts} /. Options[AlongCurve];
  DerivOpts = {Explicit, Implicit};
  If[!MemberQ[DerivOpts,opt1],
    local = Indeterminate;
    Print["AlongCurve: unknown option: ",opt1]
  ];
  If[opt1 == Explicit,local = AlongCurve1[A,Conn,c,t,Type]];
  If[opt1 == Implicit,local = AlongCurve2[A,Conn,c,t,Type]];
  local
];

AlongCurve1[A_,Conn_,c_,t_,Type_] :=
  Module[{nablaA,cprime,nablacA,n,i,j,k,r,s,Inds1,Inds2},
  r = Type[[1]];
  s = Type[[2]];
  n = Length[c];
  cprime = D[c,t];
  Inds2 = Table[0,{i,r+s+1}];
  nablacA = InitializeTensor[{r,s},n];
  nablaA = CovariantDifferential[A,Conn,c,Type,Deriv->Explicit];

  Do[
    Inds1 = IVec2Ten[i,{r,s},n];
    nablacA[[Sequence@@Inds1]] = D[A[[Sequence@@Inds1]],t];
    Do[
      Do[Inds2[[k]] = Inds1[[k]],{k,r+s}];
      Inds2[[r+s+1]] = j;
      nablacA[[Sequence@@Inds1]] = nablacA[[Sequence@@Inds1]] +
                                   nablaA[[Sequence@@Inds2]] cprime[[j]],
      {j,n}
    ],
    {i,n^(r+s)}
  ];
  nablacA
];

AlongCurve2[A_,Conn_,c_,t_,Type_] := Module[{},
  r = Type[[1]];
  s = Type[[2]];
  n = Length[c];
  cprime = D[c,t];
  Inds2 = Table[0,{i,r+s+1}];
  nablacA = InitializeTensor[{r,s},n];
  nablaA = CovariantDifferential[A,Conn,c,Type,Deriv->Implicit];

  Do[
    Inds1 = IVec2Ten[i,{r,s},n];
    nablacA[[Sequence@@Inds1]] = D[A[[Sequence@@Inds1]],t];
    Do[
      Do[Inds2[[k]] = Inds1[[k]],{k,r+s}];
      Inds2[[r+s+1]] = j;
      nablacA[[Sequence@@Inds1]] = nablacA[[Sequence@@Inds1]] +
                                   nablaA[[Sequence@@Inds2]] cprime[[j]],
      {j,n}
    ],
    {i,n^(r+s)}
  ];
  nablacA
];

(*** CovariantDerivative ***)

Options[CovariantDerivative] = {Deriv->Explicit}
CovariantDerivative[X_,Y_,Conn_,x_,opts___Rule] :=
                    Module[{opt1,local,DerivOpts},
  opt1 = Deriv /. {opts} /. Options[CovariantDerivative];
  DerivOpts = {Explicit, Implicit};
  If[!MemberQ[DerivOpts,opt1],
    local = Indeterminate;
    Print["CovariantDerivative: unknown option: ",opt1]
  ];
  If[opt1 == Explicit,local = CovarD1[X,Y,Conn,x]];
  If[opt1 == Implicit,local = CovarD2[X,Y,Conn,x]];
  local
];

CovarD1[X_,Y_,Conn_,x_] := Module[{nablaX},
  nablaX = CovariantDifferential[X,Conn,x,{1,0},Deriv->Explicit];
  EvaluateTensor[nablaX,{Y},{1,1},{{1},{}}]
];

CovarD2[X_,Y_,Conn_,x_] := Module[{nablaX},
  nablaX = CovariantDifferential[X,Conn,x,{1,0},Deriv->Implicit];
  EvaluateTensor[nablaX,{Y},{1,1},{{1},{}}]
];

(*** CovariantDifferential ***)

Options[CovariantDifferential] = {Deriv->Explicit}
CovariantDifferential[A_,Conn_,x_,Type_,opts___Rule] :=
  Module[{opt1,local,DerivOpts},
  opt1 = Deriv /. {opts} /. Options[CovariantDifferential];
  DerivOpts = {Explicit, Implicit};
  If[!MemberQ[DerivOpts,opt1],
    local = Indeterminate;
    Print["CovariantDifferential: unknown option: ",opt1]
  ];
  If[opt1 == Explicit,local = CovariantDifferential1[A,Conn,x,Type]];
  If[opt1 == Implicit,local = CovariantDifferential2[A,Conn,x,Type]];
  local
];

CovariantDifferential1[A_,Conn_,x_,Type_] :=
  Module[{r,s,n,i,j,k,m,nablaA,Inds,Inds1,Inds2,temp},
  r = Type[[1]];
  s = Type[[2]];
  n = Length[x];
  nablaA = InitializeTensor[{r,s+1},n];
  Inds1 = Table[0,{i,r+s}];

  Do[
    Inds = IVec2Ten[i,{r,s+1},n];
    Do[Inds1[[j]] = Inds[[j]],{j,r+s}];
    temp = D[A[[Sequence@@Inds1]],x[[Inds[[r+s+1]]]]];
    Do[
      Do[
        Inds2 = Inds1;
        Inds2[[k]] = m;
        temp = temp + Conn[[Inds1[[k]]]][[Inds[[r+s+1]]]][[m]]*
                      A[[Sequence@@Inds2]],
        {m,n}
      ],
      {k,r}
    ];

    Do[
      Do[
        Inds2 = Inds1;
        Inds2[[r+k]] = m;
        temp = temp - Conn[[m]][[Inds[[r+s+1]]]][[Inds1[[r+k]]]]*
                      A[[Sequence@@Inds2]],
        {m,n}
      ],
      {k,s}
    ];

    nablaA[[Sequence@@Inds]] = temp,
    {i,n^(r+s+1)}
  ];
  nablaA
];

CovariantDifferential2[A_,Conn_,x_,Type_] :=
  Module[{r,s,n,i,j,k,m,nablaA,Inds,Inds1,Inds2,temp},
  r = Type[[1]];
  s = Type[[2]];
  n = Length[x];
  nablaA = InitializeTensor[{r,s+1},n];
  Inds1 = Table[0,{i,r+s}];

  Do[
    Inds = IVec2Ten[i,{r,s+1},n];
    Do[Inds1[[j]] = Inds[[j]],{j,r+s}];
    temp = Dt[A[[Sequence@@Inds1]],x[[Inds[[r+s+1]]]]];

    Do[
      Do[
        Inds2 = Inds1;
        Inds2[[k]] = m;
        temp = temp + Conn[[Inds1[[k]]]][[Inds[[r+s+1]]]][[m]]*
                      A[[Sequence@@Inds2]],
        {m,n}
      ],
      {k,r}
    ];

    Do[
      Do[
        Inds2 = Inds1;
        Inds2[[r+k]] = m;
        temp = temp - Conn[[m]][[Inds[[r+s+1]]]][[Inds1[[r+k]]]]*
                      A[[Sequence@@Inds2]],
        {m,n}
      ],
      {k,s}
    ];

    nablaA[[Sequence@@Inds]] = temp,
    {i,n^(r+s+1)}
  ];
  nablaA
];

(*** CurvatureTensor ***)

Options[CurvatureTensor] = {Deriv->Explicit}
CurvatureTensor[Conn_,x_,opts___Rule] := Module[{opt1,local,DerivOpts},
  opt1 = Deriv /. {opts} /. Options[CurvatureTensor];
  DerivOpts = {Explicit, Implicit};
  If[!MemberQ[DerivOpts,opt1],
    local = Indeterminate;
    Print["CurvatureTensor: unknown option: ",opt1]
  ];
  If[opt1 == Explicit,local = RCurv1[Conn,x]];
  If[opt1 == Implicit,local = RCurv2[Conn,x]];
  local
];

RCurv1[Conn_,x_] := Module[{},
  Table[ D[Conn[[i]][[l]][[j]],x[[k]]]-
         D[Conn[[i]][[k]][[j]],x[[l]]]+
         Sum[Conn[[i]][[k]][[m]]Conn[[m]][[l]][[j]],{m,Length[x]}]-
         Sum[Conn[[i]][[l]][[m]]Conn[[m]][[k]][[j]],{m,Length[x]}],
         {i,Length[x]},{j,Length[x]},{k,Length[x]},{l,Length[x]}]
];

RCurv2[Conn_,x_] := Module[{},
  Table[ Dt[Conn[[i]][[l]][[j]],x[[k]]]-
         Dt[Conn[[i]][[k]][[j]],x[[l]]]+
         Sum[Conn[[i]][[k]][[m]]Conn[[m]][[l]][[j]],{m,Length[x]}]-
         Sum[Conn[[i]][[l]][[m]]Conn[[m]][[k]][[j]],{m,Length[x]}],
         {i,Length[x]},{j,Length[x]},{k,Length[x]},{l,Length[x]}]
];

(*** Grad ***)

Options[Grad] = {Deriv->Explicit}
Grad[f_,g_,x_,opts___Rule] := Module[{opt1,local,DerivOpts},
  opt1 = Deriv /. {opts} /. Options[Grad];
  DerivOpts = {Explicit, Implicit};
  If[!MemberQ[DerivOpts,opt1],
    local = Indeterminate;
    Print["Grad: unknown option: ",opt1]
  ];
  If[opt1 == Explicit,local = Grad1[f,g,x]];
  If[opt1 == Implicit,local = Grad2[f,g,x]];
  local
];

Grad1[f_,g_,x_] := Module[{},
  RiemannSharp[Table[D[f,x[[i]]],{i,Length[x]}],g]
];

Grad2[f_,g_,x_] := Module[{},
  RiemannSharp[Table[Dt[f,x[[i]]],{i,Length[x]}],g]
];

(*** LeviCivita ***)

Options[LeviCivita] = {Deriv->Explicit}
LeviCivita[g_,x_,opts___Rule] := Module[{opt1,local,DerivOpts},
  opt1 = Deriv /. {opts} /. Options[LeviCivita];
  DerivOpts = {Explicit, Implicit};
  If[!MemberQ[DerivOpts,opt1],
    local = Indeterminate;
    Print["LeviCivita: unknown option: ",opt1]
  ];
  If[opt1 == Explicit,local = LeviCivita1[g,x]];
  If[opt1 == Implicit,local = LeviCivita2[g,x]];
  local
];

LeviCivita1[g_,x_] := Module[{ginv},
  If[
     MatrixQ[g],
     If[
        Length[g] == Length[Transpose[g]],
        If[
           SameQ[Det[g],0],
           Print["The first argument of LeviCivita must be a square \
                  invertible matrix."],
           ginv = Inverse[g];
           Table[Sum[ginv[[h]][[k]](D[g[[h]][[j]],x[[i]]]+
                     D[g[[i]][[h]],x[[j]]]-D[g[[i]][[j]],x[[h]]])/2,
                     {h,Length[g]}],
                 {k,Length[g]},{j,Length[g]},{i,Length[g]}]
          ],
        Print["The first argument of LeviCivita must be a square matrix."]
       ],
     Print["The first argument of LeviCivita must be a matrix."]
    ]
];

LeviCivita2[g_,x_] := Module[{ginv},
  If[
     MatrixQ[g],
     If[
        Length[g] == Length[Transpose[g]],
        If[
           SameQ[Det[g],0],
           Print["The first argument of LeviCivita must be a square \
                  invertible matrix."],
           ginv = Inverse[g];
           Table[Sum[ginv[[h]][[k]](Dt[g[[h]][[j]],x[[i]]]+
                     Dt[g[[i]][[h]],x[[j]]]-Dt[g[[i]][[j]],x[[h]]])/2,
                     {h,Length[g]}],
                  {k,Length[g]},{j,Length[g]},{i,Length[g]}]
          ],
        Print["The first argument of LeviCivita must be a square matrix."]
       ],
     Print["The first argument of LeviCivita must be a matrix."]
    ]
];

(*** RicciCurvature ***)

Options[RicciCurvature] = {Deriv->Explicit}
RicciCurvature[Conn_,x_,opts___Rule] := Module[{opt1,local,DerivOpts},
  opt1 = Deriv /. {opts} /. Options[RicciCurvature];
  DerivOpts = {Explicit, Implicit};
  If[!MemberQ[DerivOpts,opt1],
    local = Indeterminate;
    Print["RicciCurvature: unknown option: ",opt1]
  ];
  If[opt1 == Explicit,local = Ricci1[Conn,x]];
  If[opt1 == Implicit,local = Ricci2[Conn,x]];
  local
];

Ricci1[Conn_,x_] := Module[{R},
  R = CurvatureTensor[Conn,x,Deriv->Explicit];
  Table[Sum[R[[k]][[i]][[k]][[j]],{k,Length[x]}],
        {i,Length[x]},{j,Length[x]}]
];

Ricci2[Conn_,x_] := Module[{R},
  R = CurvatureTensor[Conn,x,Deriv->Implicit];
  Table[Sum[R[[k]][[i]][[k]][[j]],{k,Length[x]}],
        {i,Length[x]},{j,Length[x]}]
];

(*** RiemannFlat ***)

RiemannFlat[X_,g_] := Module[{},
  EvaluateTensor[g,{X},{0,2},{{},{1}}]
];

(*** RiemannSharp ***)

RiemannSharp[w_,g_] := Module[{ginv},
  ginv = Inverse[g];
  EvaluateTensor[ginv,{w},{2,0},{{1},{}}]
];

(*** ScalarCurvature ***)

Options[ScalarCurvature] = {Deriv->Explicit};
ScalarCurvature[g_,x_,opts___Rule] := Module[{opt1,local,DerivOpts},
  opt1 = Deriv /. {opts} /. Options[ScalarCurvature];
  DerivOpts = {Explicit, Implicit};
  If[!MemberQ[DerivOpts,opt1],
    local = Indeterminate;
    Print["ScalarCurvature: unknown option: ",opt1]
  ];
  If[opt1 == Explicit,local = ScalarCurv1[g,x]];
  If[opt1 == Implicit,local = ScalarCurv2[g,x]];
  local
];

ScalarCurv1[g_,x_] := Module[{Conn,Ric,ginv},
  Conn = LeviCivita[g,x,Deriv->Explicit];
  Ric = RicciCurvature[Conn,x,Deriv->Explicit];
  ginv = Inverse[g];
  Sum[Sum[ginv[[i]][[j]]Ric[[i]][[j]],{j,Length[g]}],{i,Length[g]}]
];

ScalarCurv2[g_,x_] := Module[{Conn,Ric,ginv},
  Conn = LeviCivita[g,x,Deriv->Implicit];
  Ric = RicciCurvature[Conn,x,Deriv->Implicit];
  ginv = Inverse[g];
  Sum[Sum[ginv[[i]][[j]]Ric[[i]][[j]],{j,Length[g]}],{i,Length[g]}]
];

(*** SectionalCurvature ***)

Options[SectionalCurvature] = {Deriv->Explicit}
SectionalCurvature[X_,Y_,g_,x_,opts___Rule] :=
                   Module[{opt1,local,DerivOpts},
  opt1 = Deriv /. {opts} /. Options[SectionalCurvature];
  DerivOpts = {Explicit, Implicit};
  If[!MemberQ[DerivOpts,opt1],
    local = Indeterminate;
    Print["SectionalCurvature: unknown option: ",opt1]
  ];
  If[opt1 == Explicit,local = SectionalCurv1[X,Y,g,x]];
  If[opt1 == Implicit,local = SectionalCurv2[X,Y,g,x]];
  local
];

SectionalCurv1[X_,Y_,g_,x_] := Module[{R,Conn},
  Conn = LeviCivita[g,x,Deriv->Explicit];
  R = CurvatureTensor[Conn,x,Deriv->Explicit];
  If[
     SameQ[Y.g.X,0],
     If[
        SameQ[X.g.X,1],
        If[
           SameQ[Y.g.Y,1],
           Sum[Sum[Sum[Sum[Sum[
           g[[m]][[i]] R[[i]][[j]][[k]][[l]]X[[k]]Y[[l]]Y[[j]]X[[m]],
           {i,Length[g]},{j,Length[g]},{k,Length[g]},
           {l,Length[g]},{m,Length[g]}]]]]],
           Print["The first argument must be a unit vector."]
          ],
        Print["The second argument must be a unit vector."]
       ],
     Print["The first two arguments must be orthogonal."]
    ]
];

SectionalCurv2[X_,Y_,g_,x_] := Module[{R,Conn},
  Conn = LeviCivita[g,x,Deriv->Implicit];
  R = CurvatureTensor[Conn,x,Deriv->Implicit];
  If[
     SameQ[Y.g.X,0],
     If[
        SameQ[X.g.X,1],
        If[
           SameQ[Y.g.Y,1],
           Sum[Sum[Sum[Sum[Sum[
           g[[m]][[i]]R[[i]][[j]][[k]][[l]]X[[k]]Y[[l]]Y[[j]]X[[m]],
           {i,Length[g]},{j,Length[g]},{k,Length[g]},
           {l,Length[g]},{m,Length[g]}]]]]],
           Print["The first argument must be a unit vector."]
          ],
        Print["The second argument must be a unit vector."]
       ],
     Print["The first two arguments must be orthogonal."]
    ]
];

(*** Spary ***)

Spray[Conn_,x_,xdot_] := Module[{verpart},
  If[
     VectorQ[x],
     If[
        VectorQ[xdot],
        verpart = Table[Sum[Sum[
                  -Conn[[i]][[j]][[k]]xdot[[j]]xdot[[k]],
                  {k,Length[x]}],{j,Length[x]}],{i,Length[x]}];
        Flatten[Join[xdot,verpart]],
        Print["The third argument of Spray must be a vector."]
       ],
     Print["The second argument of Spray must be a vector."]
    ]
];

(*** SymmetricProduct ***)

Options[SymmetricProduct] = {Deriv->Explicit}
SymmetricProduct[X_,Y_,Conn_,x_,opts___Rule] :=
                 Module[{opt1,local,DerivOpts},
  opt1 = Deriv /. {opts} /. Options[SymmetricProduct];
  DerivOpts = {Explicit, Implicit};
  If[!MemberQ[DerivOpts,opt1],
    local = Indeterminate;
    Print["SymmetricProduct: unknown option: ",opt1]
  ];
  If[opt1 == Explicit,local = SymProd1[X,Y,Conn,x]];
  If[opt1 == Implicit,local = SymProd2[X,Y,Conn,x]];
  local
];

SymProd1[X_,Y_,Conn_,x_] := Module[{},
  CovariantDerivative[X,Y,Conn,x,Deriv->Explicit] +
  CovariantDerivative[Y,X,Conn,x,Deriv->Explicit]
];

SymProd2[X_,Y_,Conn_,x_] := Module[{},
  CovariantDerivative[X,Y,Conn,x,Deriv->Implicit] +
  CovariantDerivative[Y,X,Conn,x,Deriv->Implicit]
];

(*** TorsionTensor ***)

TorsionTensor[Conn_,x_] := Module[{},
  Table[Conn[[i]][[j]][[k]]-Conn[[i]][[k]][[j]],
        {i,Length[x]},{j,Length[x]},{k,Length[x]}]
];

End[]
EndPackage[]
