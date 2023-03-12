(*
 * SMCS.m
 *
 * Andrew D. Lewis
 * Queen's University
 * 28 July 2004
 *
 * Modules for modeling of simple mechanical (control) systems.
 *
 *)

BeginPackage["SMCS`",{"Affine`","Tensors`"}]

ACCSequations::usage = "ACCSequations[conn,Y,q,t] gives the equations of
motion for an affine connection control system with Christoffel symbols conn,
input vector field Y, coordinates q, and with time t."

ACCSsimulate::usage = "ACCSsimulate[eqns,q,qinit,vinit,t,Ti,Tf] simulates the
equations of motion for the affine connection control system with forced
geodesic equations eqns, configuration q, initial configuration qinit,
initial velocity vinit, time variable t, initial time Ti, and final time Tf."

BodyAngularVelocity::usage = "BodyAngularVelocity[R_,q_,t_] returns the body
angular velocity in coordinates q, with t being the time variable, and
with R being the orientation of the system at coordinate q.  A three-vector
is returned."

ConstrainedConnection::usage = "ConstrainedConnection[conn,A,P,q]
computes the constrained connection derived from the connection with
Christoffel symbols conn and the complementary projection P in coordinates q.
A is an arbitrary invertible matrix function.";

Force::usage = "Force[tau_,F_,R_,r_,q_,t_] produces the Lagrangian force
corresponding to a Newtonian force applied at the center of mass, and the
torque tau applied about the center of mass.  Such a force will generally
interact with multiple bodies, and this command is designed to produce the
Lagrangian force arising from all bodies impacted by the Newtonian force and
torque.  The first four arguments are therefore lists whose length is the
number of bodies impacted by the Newtonian force and torque.  tau and F are
lists of three-dimensional vectors which are the Newtonian torques and
forces, respectively.  R is a list of 3x3 rotation matrices which, along with
the list of three-dimensional vectors r, defines the local representatives of
the forward kinematic maps q |-> (R,r) in coordinates q.  Of course, the
order in these four lists should be consistent.  The time variable is t."

GeneralizedCovariantDerivative::usage =
"GeneralizedCovariantDerivative[X,Y,A,conn,x] gives the covariant derivative
of a vector field Y with respect to a vector field Y.  It is assumed that
conn are the generalized Christoffel symbols with respect to some basis of
vector fields which form the entries of A, and that X and Y are the
components of the vector fields with respect to this basis.  Thus X and Y
must represent vector fields lying in the constraint distribution.  The
coordinates are x."

GetState::usage = "GetState[X,pv,q,t] returns the states for a system with
pseudovelovities pv relative to a basis of vector fields X, and with
coordinates q.  The time is t."

Hat::usage = "Hat[omega] returns the element in so(3) corresponding to omega
in R^3."

Hessian::usage = "Hessian[f,x] returns the Hessian of the function f with
respect to the coordinates x."

KErot::usage = "KErot[R,q,Ic,t] gives the rotational kinetic energy of a body
with inertia tensor Ic about its center of mass.  R is a proper orthogonal
matrix obtained as follows.  Choose an inertial frame {e1,e2,e3} and a body
frame {f1,f2,f3}, the latter having origin at the center of mass.  The matrix
R is then the matrix whose ith column, i=1,2,3, are the components of fi
relative to the basis {e1,e2,e3}.  These components should be expressed as
functions of the coordinates q for the configuration space.  The coordinates
should be specified as functions of the time variable t.  Thus
q={q1[t],...,qn[t]}.  Note that the inertia tensor is defined relative to the
frame {f1,f2,f3}."

KEtrans::usage = "KEtrans[r,q,m,t] gives the translational kinetic energy of
a body of mass m.  The vector r gives the position of the centre of mass of
the body relative to an inertial frame, with the position being expressed as
a 3-vector with components functions of the coordinates q for the
configuration space.  The coordinates should be specified as functions of the
time variable t.  Thus q={q1[t],...,qn[t]}."

KE2Metric::usage = "KE2Metric[KE,qdot] returns the components of the
Riemannian metric which gives the kinetic energy KE.  qdot are the natural
velocity coordinates."

OrthogonalChristoffelSymbols::usage =
"OrthogonalChristoffelSymbols[X,g,conn,x] computes the generalized
Christoffel symbols for the orthogonal vector fields contained in the columns
of X.  Here g is the matrix for the Riemannian metric, conn are the
Christoffel symbols of the Levi-Civita connection, and x are the coordinates."

OrthogonalForce::usage = "OrthogonalForce[F,X,g] returns the compnents of the
vector force associated with the force F, projected onto the distribution
spanned by the vector fields forming the entries in X, and resolved into the
components relative to these vector fields.  The vector fields in X are
assumed g-orthogonal."

OrthogonalProjection::usage = "OrthogonalProjection[A,g] computes the
orthogonal projection onto the subspace generated by the vectors in A with
respect to the inner product with components g.  For the results to be
meaningful, the objects in A must be orthonormal."

SetEqual::usage = "SetEqual[a,b] sets up an equation where the components of
a are equal to those of b, i.e., a == b."

SMCSequations::usage = "SMCSequations[conn,Y,X,pv,q,t] gives equations of
motion for a simple mechanical (control) system with constraints, possibly
using generalized Christoffel symbols.  The possibly generalized Christoffel
symbols are conn.  X is the basis of the constraint distribution, not
necessarily orthogonal.  Y is the vector force, projected to the constraint
distribution, and given as components in the vector fields in X.  pv is a
full list of the pseudo-velocities, q is the configuration, and t is the
time."

SMCSsimulate::usage = "SMCSsimulate[eqns,X,q,pv,qinit,vinit,t,Ti,Tf]
simulates the equations of motion for a simple mechanical control system,
possibly with constraints generated by the vector fields in the list X.  The
equations are eqns, the configurations are q, and the complete list of
pseudo-velocities are pv.  The initial configuration is qinit, and the
initial velocity is vinit.  Note that the initial velocity is the natural
initial velocity, not the initial velocity for the pseudo-velocities.  The
program converts the natural velocity initial conditions to pseudo-velocity
initial conditions.  The time variable is t, the initial time is Ti, and the
final time is Tf."

SpatialAngularVelocity::usage = "BodyAngularVelocity[R_,q_,t_] returns the
spatial angular velocity in coordinates q, with t being the time variable,
and with R being the orientation of the system at coordinate q.  A
three-vector is returned."

Unhat::usage = "Unhat[omegahat] returns the vector in R^3 corresponding to
the matrix omegahat in so(3)."

Begin["Private`"]

Print["\nPackage \"SMCS\" defines: ACCSequations, ACCSsimulate, BodyAngularVelocity, ConstrainedConnection, Force, GeneralizedCovariantDerivative, GetState, Hat, Hessian, KErot, KEtrans, OrthogonalChristoffelSymbols, OrthogonalForce, OrthogonalProjection, SetEqual, SMCSequations, SMCSsimulate, SpatialAngularVelocity, Unhat."];

(*** ACCSequations ***)

ACCSequations[conn_,Y_,q_,t_] := Module[{vel},
  vel = D[q,t];
  SetEqual[AlongCurve[vel,conn,q,t,{1,0}],Y]
]

(*** ACCSsimulate ***)

ACCSsimulate[eqns_,q_,qinit_,vinit_,t_,Ti_,Tf_] := Module[{inits},
  inits = Join[SetEqual[q/.t->Ti,qinit],SetEqual[D[q,t]/.t->Ti,vinit]];
  NDSolve[Join[eqns,inits],q,{t,Ti,Tf}]
]

(*** BodyAngularVelocity ***)

BodyAngularVelocity[R_,q_,t_] := Module[{},
  Unhat[Transpose[R].D[R,t]]
]

(*** ConstrainedConnection ***)

ConstrainedConnection[conn_,A_,P_,q_] := Module[{B,Ainv,i,j,k,l,m},
  B = A.P;
  Ainv = Inverse[A];
  Table[conn[[i,k,j]]+Sum[Ainv[[i,m]] D[B[[m,j]],q[[k]]],
             {m,Length[q]}]+
         Sum[Ainv[[i,m]] Sum[conn[[m,k,l]] B[[l,j]],
             {l,Length[q]}],{m,Length[q]}]-
         Sum[Ainv[[i,m]] Sum[conn[[m,k,j]] B[[i,l]],
             {l,Length[q]}],{m,Length[q]}],
         {i,Length[q]},{j,Length[q]},{k,Length[q]}]  
]

(*** GeneralizedCovariantDerivative ***)

GeneralizedCovariantDerivative[X_,Y_,A_,conn_,x_] := Module[{i,j,k},
  Table[
    Sum[Sum[conn[[i,j,k]]X[[j]]Y[[k]],{j,Length[A]}],{k,Length[A]}]+
    Sum[X[[j]]LieDerivative[Y[[i]],A[[j]],x,{0,0}],{j,Length[A]}],
    {i,Length[A]}
  ]
]

(*** GetState ***)

GetState[X_,pv_,q_,t_] :=
  Module[{state,qredun,Ddim,i,j,iflag,Qdim,qeqn1,pvc,pvt},
  Ddim = Length[X];
  Qdim = Length[q];
  state = q;
  If[Length[pv] == 0,pvt = Table[pvc[i][t],{i,Qdim}],pvt = pv];
  qeqn1 = SetEqual[D[q,t],Sum[pvt[[i]]X[[i]],{i,Ddim}]];
  qredun = {};
  Do[
    Do[
      If[qeqn1[[i]]==(D[q[[i]],t]==pvt[[j]]),
        qredun=Join[qredun,{{i,j}}]
      ],
      {j,Ddim}
    ],
    {i,Qdim}
  ];
  Do[
    iflag = 0;
    Do[
      If[i==qredun[[j,2]],iflag=1],
      {j,Length[qredun]}
    ];
    If[iflag==0,state=Join[state,{pvt[[i]]}]],
    {i,Ddim}
  ];
  state
]

(*** Force ***)

Force[tau_,F_,R_,r_,q_,t_] := Module[{qdot,omega,power,ELF,i},
  omega = Table[SpatialAngularVelocity[R[[i]],q,t],{i,Length[F]}];
  qdot = D[q,t];
  power = Sum[F[[i]].D[r[[i]],t]+tau[[i]].omega[[i]],{i,Length[F]}];
  ELF = Table[Coefficient[power,qdot[[i]]],{i,Length[q]}];
  ELF
]

(*** Hat ***)

Hat[omega_] := Module[{},
  {{0,-omega[[3]],omega[[2]]},
   {omega[[3]],0,-omega[[1]]},
   {-omega[[2]],omega[[1]],0}}
]

(*** Hessian ***)

Hessian[f_,x_] := Module[{},
  Table[D[f,x[[i]],x[[j]]],{i,Length[x]},{j,Length[x]}]
]

(*** KErot ***)

KErot[R_,q_,Ic_,t_] := Module[{Omega},
  Omega = BodyAngularVelocity[R,q,t];
  (1/2)(Ic.Omega).Omega
];

(*** KEtrans ***)

KEtrans[r_,q_,m_,t_] := Module[{},
  (1/2)m(D[r,t].D[r,t])
]

(*** KE2Metric ***)

KE2Metric[KE_,qdot_] := Module[{i,j,n},
  n = Length[qdot];
  Table[D[KE,qdot[[i]],qdot[[j]]],{i,n},{j,n}]
]

(*** OrthogonalChristoffelSymbols ***)

OrthogonalChristoffelSymbols[X_,g_,conn_,x_] := Module[{k},
  k = Length[X];
  Table[(CovariantDerivative[X[[j]],X[[l]],conn,x].g.X[[i]])/
        (X[[i]].g.X[[i]]),{i,k},{j,k},{l,k}]
]

(*** OrthogonalForce ***)

OrthogonalForce[F_,X_,g_] := Module[{i},
  Table[F.X[[i]]/(X[[i]].g.X[[i]]),{i,Length[X]}]
]

(*** OrthogonalProjection ***)

OrthogonalProjection[A_,g_] := Module[{v,a,B},
  v = Table[a[i],{i,Length[g]}];
  B = Sum[A[[i]].g.v A[[i]],{i,Length[A]}];
  Table[Coefficient[Expand[B[[i]]],a[j]],{i,Length[g]},{j,Length[g]}]
]

(*** SetEqual ***)

SetEqual[a_,b_] := Module[{n,i},
  n=Evaluate[Dimensions[a]][[1]]; 
  Table[a[[i]]==b[[i]],{i,1,n}]
]

(*** SMCSequations ***)

SMCSequations[conn_,Y_,X_,pv_,q_,t_] := Module[{qeqn1,pveqn,i,j,k,Ddim,Qdim,
  state,qredun,qeqn,eqns,vel,iflag,pvc,pvt},
  If[Length[pv] == 0,pvt = Table[pvc[i][t],{i,Qdim}],pvt = pv];
  Qdim = Length[q];
  Ddim = Length[pvt];
  qeqn1 = SetEqual[D[q,t],Sum[pvt[[i]]X[[i]],{i,Ddim}]];
  pveqn = SetEqual[D[pvt,t]+
          Table[Sum[conn[[i,j,k]]pvt[[j]]pvt[[k]],{j,Ddim},{k,Ddim}],{i,Ddim}],
          Y];
  state = q;
  vel = D[q,t];
  qredun = {};
  qeqn = {};
  Do[
    Do[
      If[qeqn1[[i]]==(D[q[[i]],t]==pvt[[j]]),
        qredun = Join[qredun,{{i,j}}]
      ],
      {j,Ddim}
    ],
    {i,Qdim}
  ];
  Do[
    iflag = 0;
    Do[
      If[i==qredun[[j,1]],iflag=j],
      {j,Length[qredun]}
    ];
    If[iflag==0,qeqn=Join[qeqn,{qeqn1[[i]]}]],
    {i,Qdim}
  ];
  Do[
    iflag = 0;
    Do[
      If[i==qredun[[j,2]],iflag=1],
      {j,Length[qredun]}
    ];
    If[iflag==0,state=Join[state,{pvt[[i]]}]],
    {i,Ddim}
  ];
  eqns = Join[qeqn,pveqn];
  Do[
    eqns = eqns/.pvt[[qredun[[i,2]]]]->vel[[qredun[[i,1]]]];
    eqns = eqns/.D[pvt[[qredun[[i,2]]]],t]->D[vel[[qredun[[i,1]]]],t],
    {i,Length[qredun]}
  ];
  eqns
]

(*** SMCSsimulate ***)

SMCSsimulate[eqns_,X_,q_,pv_,qinit_,pvinit_,t_,Ti_,Tf_] :=
  Module[{qinits,pvinits,inits,Ddim,qeqn1,vel,iflag,qredun},
  Ddim = Length[X];
  Qdim = Length[q];
  If[Length[pv] == 0,pvt = Table[pvc[i][t],{i,Qdim}],pvt = pv];
  qeqn1 = SetEqual[D[q,t],Sum[pvt[[i]]X[[i]],{i,Ddim}]];
  state = GetState[X,pvt,q,t];
  vel = D[q,t];
  qredun = {};
  Do[
    Do[
      If[qeqn1[[i]]==(D[q[[i]],t]==pvt[[j]]),
        qredun=Join[qredun,{{i,j}}]
      ],
      {j,Ddim}
    ],
    {i,Qdim}
  ];
  qinits = SetEqual[q/.t->0,qinit];
  pvinits = {};
  Do[
    iflag = 0;
    Do[
      If[i==qredun[[j,2]],iflag=j],
      {j,Length[qredun]}
    ];
    If[iflag==0,
      pvinits = Join[pvinits,{(pvt[[i]]/.t->0)==pvinit[[i]]}],
      pvinits = Join[pvinits,
                  {(vel[[qredun[[iflag,1]]]]/.t->0)==pvinit[[i]]}];
    ],
    {i,Ddim}
  ];
  inits = Join[qinits,pvinits];
  NDSolve[Join[eqns,inits],state,{t,Ti,Tf}]
]

(*** SpatialAngularVelocity ***)

SpatialAngularVelocity[R_,q_,t_] := Module[{},
  Unhat[D[R,t].Transpose[R]]
]

(*** Unhat ***)

Unhat[omegahat_] := Module[{},
  {omegahat[[3,2]],omegahat[[1,3]],omegahat[[2,1]]}
]

End[]
EndPackage[]
