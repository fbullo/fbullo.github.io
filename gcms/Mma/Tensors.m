(*
 * Tensors.m
 *
 * Andrew D. Lewis
 * Queen's University
 * 24 August 1999
 *
 * Modules for tensor manipulation.
 *
 *)

BeginPackage["Tensors`",{"DerivativeOptions`"}]

ChangeBasis::usage = "ChangeBasis[A,P,Type] provides the components for the
type Type tensor obtain from A via the change of basis matrix P.  The rule
for P is as follows.  If the old basis is e[[i]] and the new basis is f[[j]]
then f[[j]]=P[[i]][[j]] e[[i]].  Thus the columns of P are the vectors f in
the basis e.";

ChangeCoordinates::usage = "ChangeCoordinates[A,x,xp,xofxp,Type,Deriv] gives
a tensor A of type Type expressed originally in coordinates x, in coordinates
xp.  Here xofxp gives x as a function of xp.  If Type=\"Affine Connection\"
then the input should be the Christoffel symbols of an affine connection in
coordinates x, and the result will be the Christoffel symbols in coordinates
xp.";

EvaluateTensor::usage = "EvaluateTensor[A,arguments,Type,free] evaluates
tensor A of type Type on arguments.  free is a list of two (possibly empty)
lists, the first giving the contravariant (i.e., up) indices which are to be
left free, and the second giving the covariant (i.e., down) indices to be
left free.";

InitializeTensor::usage = "InitializeTensor[{r,s},n] makes a tensor of depth
k in n variables with all zeroes.";

ITen2Vec::usage = "ITen2Vec[Inds,n] takes the indices Inds of a (r,s)-tensor
in n-dimensions and returns a n^(r+s) vector of positive integers.";

IVec2Ten::usage = "IVec2Ten[Ind,Type,n] takes item Ind of a list of n^k
positive integers and returns indices for a (r,s)-tensor in n-dimensions,
where Type={r,s}.";

LieDerivative::usage = "LieDerivative[A,X,x,Type,Deriv] gives the Lie
derivative of the tensor field A of type Type with respect to the vector
field X in coordinates x.";

Ten2Vec::usage = "Te2Vec[A,Type,n] takes a (r,s)-tensor A in n-dimensions and
returns a n^k vector, where Type={r,s}";

TheJacobian::usage = "TheJacobian[f,x] returns the Jacobian of the map f with
respect to the coordinates x."

Vec2Ten::usage = "Vec2Ten[Avect,Type,n] takes a n^(r+s) vector Avect and
returns a (r,s)-tensor in n-dimensions, where Type={r,s}.";

Begin["`Private`"]

Print["\nPackage \"Tensors\" defines: ChangeBasis, ChangeCoordinates, EvaluateTensor, InitializeTensor, ITen2Vec, IVec2Ten, LieDerivative, Ten2Vec, TheJacobian, Vec2Ten."];

(*** ChangeBasis ***)

ChangeBasis[A_,P_,Type_] :=
  Module[{Pinverse,r,s,n,Anew,i,j,k,l,Inds1,Inds2,temp1,temp2},
  Pinverse = Inverse[P];
  r = Type[[1]];
  s = Type[[2]];
  n = Length[P];
  Anew = InitializeTensor[{r,s},n];

  Do[
    temp1 = 0;
    Do[
      Inds1 = IVec2Ten[i,{r,s},n];
      Inds2 = IVec2Ten[j,{r,s},n];
      temp2 = A[[Sequence@@Inds2]];
      Do[
        temp2 = temp2*Pinverse[[Inds1[[k]]]][[Inds2[[k]]]],
        {k,r}
      ];
      Do[
        temp2 = temp2*P[[Inds2[[r+l]]]][[Inds1[[r+l]]]],
        {l,s}
      ];
      temp1 = temp1+temp2,
      {j,n^(r+s)}
    ];
    Anew[[Sequence@@Inds1]] = temp1,
    {i,n^(r+s)}
  ];
  Anew
];

(*** ChangeCoordinates ***)

Options[ChangeCoordinates] = {Deriv->Explicit}
ChangeCoordinates[A_,x_,xp_,xofxp_,Type_,opts___Rule] :=
                  Module[{opt1,local,DerivOpts},
  opt1 = Deriv /. {opts} /. Options[ChangeCoordinates];
  DerivOpts = {Explicit, Implicit};
  If[!MemberQ[DerivOpts, opt1],
    local = Indeterminate;
    Print["ChangeCoordinates: unknown option: ", opt1]
  ];
  If[opt1 == Explicit, local = ChangeCoordinates1[A,x,xp,xofxp,Type]];
  If[opt1 == Implicit, local = ChangeCoordinates2[A,x,xp,xofxp,Type]];
  local
  ];

ChangeCoordinates1[A_,x_,xp_,xofxp_,Type_] :=
  Module[{r,p,q,i,j,k,P,Pinv,Ap,Anew},
  P = Table[D[xofxp[[i]],xp[[j]]],{i,Length[x]},{j,Length[x]}];
  If[StringQ[Type],
    Pinv = Inverse[P];
    Anew = Table[Sum[A[[r,p,q]]*P[[p]][[i]]*P[[q]][[j]]*Pinv[[k]][[r]],
                     {r,Length[x]},{p,Length[x]},{q,Length[x]}]+
                 Sum[D[xofxp[[p]],xp[[i]],xp[[j]]] Pinv[[k]][[p]],
                     {p,Length[x]}],
                 {k,Length[x]},{i,Length[x]},{j,Length[x]}];
    Do[Anew = Anew/.x[[i]]->xofxp[[i]],{i,Length[x]}],
    Ap = A;
    Do[
      Ap = Ap/.x[[i]]->xofxp[[i]];
      P = P/.x[[i]]->xofxp[[i]],
      {i,Length[x]}
    ];
    Anew = ChangeBasis[Ap,P,Type]
  ];
  Anew
];

ChangeCoordinates2[A_,x_,xp_,xofxp_,Type_] :=
  Module[{r,p,q,i,j,k,P,Pinv,Ap,Anew},
  P = Table[Dt[xofxp[[i]],xp[[j]]],{i,Length[x]},{j,Length[x]}];
  If[StringQ[Type],
    Pinv = Inverse[P];
    Anew = Table[Sum[A[[r,p,q]]*P[[p]][[i]]*P[[q]][[j]]*Pinv[[k]][[r]],
                     {r,Length[x]},{p,Length[x]},{q,Length[x]}]+
                 Sum[Dt[xofxp[[p]],xp[[i]],xp[[j]]] Pinv[[k]][[p]],
                     {p,Length[x]}],
                 {k,Length[x]},{i,Length[x]},{j,Length[x]}];
    Do[Anew = Anew/.x[[i]]->xofxp[[i]],{i,Length[x]}],
    Ap = A;
    Do[
      Ap = Ap/.x[[i]]->xofxp[[i]];
      P = P/.x[[i]]->xofxp[[i]],
      {i,Length[x]}
    ];
    Anew = ChangeBasis[Ap,P,Type]
  ];
  Anew
];

(*** EvaluateTensor ***)

EvaluateTensor[A_,arguments_,Type_,free_] :=
  Module[{r,s,upfree,downfree,upsummed,downsummed,i,j,k,jfree,jsummed,Anew,
          Inds1,Inds2,Inds3,temp1,temp2},
  r = Type[[1]];
  s = Type[[2]];
  n = Length[A];
  upfree = Sort[free[[1]]];
  downfree = Sort[free[[2]]];
  upsummed = Complement[Table[i,{i,r}],upfree];
  downsummed = Complement[Table[i,{i,s}],downfree];
  Anew = InitializeTensor[{Length[upfree],Length[downfree]},n];
  Inds3 = Table[0,{i,r+s}];

  Do[
    temp1 = 0;
    Do[
      Inds1 = IVec2Ten[i,{Length[upfree],Length[downfree]},n];
      Inds2 = IVec2Ten[j,{Length[upsummed],Length[downsummed]},n];
      Do[
        Inds3[[upfree[[k]]]] = Inds1[[k]],
        {k,Length[upfree]}
      ];
      Do[
        Inds3[[upsummed[[k]]]] = Inds2[[k]],
        {k,Length[upsummed]}
      ];
      Do[
        Inds3[[r+downfree[[k]]]] = Inds1[[Length[upfree]+k]],
        {k,Length[downfree]}
      ];
      Do[
        Inds3[[r+downsummed[[k]]]] = Inds2[[Length[upsummed]+k]],
        {k,Length[downsummed]}
      ];
      temp2 = A[[Sequence@@Inds3]];
      Do[
        temp2 = temp2*arguments[[k]][[Inds2[[k]]]],
        {k,Length[upsummed]}
      ];
      Do[
        temp2 = temp2*
           arguments[[Length[upsummed]+k]][[Inds2[[Length[upsummed]+k]]]],
        {k,Length[downsummed]}
      ];
      temp1 = temp1+temp2,
      {j,n^(Length[upsummed]+Length[downsummed])}
    ];
    Anew[[Sequence@@Inds1]] = temp1,
    {i,n^(Length[upfree]+Length[downfree])}
  ];
  Anew
];

(*** InitializeTensor ***)

InitializeTensor[Type_, n_] := Module[{A,i,j,k},
  k = Type[[1]]+Type[[2]];
  A = Table[0,{i,n}];
  Do[A=Table[A,{i,n}],{j,k-1}];
  A
];

(*** ITen2Vec ***)

ITen2Vec[Inds_, n_] := Module[{k},
  k = Length[Inds];
  Inds[[k]] + Sum[(Inds[[j]]-1)n^j,{j,k-1}]
];

(*** IVec2Ten ***)

IVec2Ten[Ind_, Type_, n_] := Module[{flag,i,j,l,inds,num,k},
  k = Type[[1]]+Type[[2]];
  inds = Table[0,{i,k}];
  num = 0;
  Do[
    flag = 0;
    inds[[k-j]] = 1;
    Do[
      If[flag == 0,
        If[Ind-num <= l n^(k-j),
          flag = 1;
          inds[[k-j]] = l
        ];
      ],
      {l,n}
    ];
    num = num + (inds[[k-j]]-1) n^(k-j),
    {j,k-1}
  ];
  inds[[k]] = Ind-num;
  inds
];

(*** LieDerivative ***)

Options[LieDerivative] = {Deriv->Explicit}
LieDerivative[A_,X_,x_,Type_,opts___Rule] := Module[{opt1,local,DerivOpts},
  opt1 = Deriv /. {opts} /. Options[LieDerivative];
  DerivOpts = {Explicit, Implicit};
  If[!MemberQ[DerivOpts, opt1],
    local = Indeterminate;
    Print["LieDerivative: unknown option: ", opt1]
  ];
  If[opt1 == Explicit, local = LieDerivative1[A,X,x,Type]];
  If[opt1 == Implicit, local = LieDerivative2[A,X,x,Type]];
  local
];

LieDerivative1[A_,X_,x_,Type_] :=
  Module[{r,s,n,i,j,k,m,lieA,Inds,Inds1,Inds2,temp},
  r = Type[[1]];
  s = Type[[2]];
  n = Length[x];
  lieA = InitializeTensor[{r,s},n];
  Inds1 = Table[0,{i,r+s}];

  Do[
    Inds = IVec2Ten[i,{r,s},n];
    Do[Inds1[[j]] = Inds[[j]],{j,r+s}];
    temp = Sum[D[A[[Sequence@@Inds1]],x[[m]]]X[[m]],{m,n}];
    Do[
      Do[
        Inds2 = Inds1;
        Inds2[[k]] = m;
        temp = temp-A[[Sequence@@Inds2]]*D[X[[Inds1[[k]]]],x[[m]]],
        {m,n}
      ],
      {k,r}
    ];

    Do[
      Do[
        Inds2 = Inds1;
        Inds2[[r+k]] = m;
        temp = temp-A[[Sequence@@Inds2]]*D[X[[m]],x[[Inds1[[r+k]]]]],
        {m,n}
      ],
      {k,s}
    ];
    lieA[[Sequence@@Inds]] = temp,
    {i,n^(r+s)}
  ];
  lieA
];

LieDerivative2[A_,X_,x_,Type_] :=
  Module[{r,s,n,i,j,k,m,lieA,Inds,Inds1,Inds2,temp},
  r = Type[[1]];
  s = Type[[2]];
  n = Length[x];
  lieA = InitializeTensor[{r,s},n];
  Inds1 = Table[0,{i,r+s}];

  Do[
    Inds = IVec2Ten[i,{r,s},n];
    Do[Inds1[[j]] = Inds[[j]],{j,r+s}];
    temp = Sum[Dt[A[[Sequence@@Inds1]],x[[m]]]X[[m]],{m,n}];
    Do[
      Do[
        Inds2 = Inds1;
        Inds2[[k]] = m;
        temp = temp-A[[Sequence@@Inds2]]*Dt[X[[Inds1[[k]]]],x[[m]]],
        {m,n}
      ],
      {k,r}
    ];

    Do[
      Do[
        Inds2 = Inds1;
        Inds2[[r+k]] = m;
        temp = temp-A[[Sequence@@Inds2]]*Dt[X[[m]],x[[Inds1[[r+k]]]]],
        {m,n}
      ],
      {k,s}
    ];
    lieA[[Sequence@@Inds]] = temp,
    {i,n^(r+s)}
  ];
  lieA
];

(*** TheJacobian ***)

TheJacobian[f_,x_] := Module[{i,j},
  Table[D[f[[i]],x[[j]]],{i,Length[f]},{j,Length[x]}]
];

(*** Ten2Vec ***)

Ten2Vec[A_,Type_,n_] := Module[{i,Inds,Avect,k},
  k = Type[[1]]+Type[[2]];
  Avect = Table[0,{i,n^k}];
  Do[
    Inds = IVec2Ten[i,Type,n];
    Avect[[i]] = A[[Sequence@@Inds]],
    {i,n^k}
  ];
  Avect
];

(*** Vec2Ten ***)

Vec2Ten[Avect_,Type_,n_] := Module[{A,i,Inds,k},
  k = Type[[1]]+Type[[2]];
  A = InitializeTensor[Type,n];
  Do[
    Inds = IVec2Ten[i,Type,n];
    A[[Sequence@@Inds]] = Avect[[i]],
    {i,n^k}
  ];
  A
];

End[]
EndPackage[]
