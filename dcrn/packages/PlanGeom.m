(*
 *     PlanGeom.m - a Mathematica Package 
 *     "Some Planar Geometry Primitives"
 *
 *     Jorge Cortes and Francesco Bullo
 *	
 *     Applied Mathematics and Statistics
 *     University of California at Santa Cruz
 *	
 *     Mechanical and Environmental Engineering
 *     University of California at Santa Barbara
 * 			   
 * Copyright (c) 2004, 2005, Francesco Bullo, Jorge Cortes
 * Permission is granted to copy, modify and redistribute this file, 
 * provided that this header message is retained.
 *
 * Revision history:
 *
 * Version 0.04 (October 05)
 * Version 0.03 (July 04)
 * Version 0.02 (July 04)
 * Version 0.01 (June 04)
 *
 * Convention: {distance, point}, e.g., {incenter, inradius}.
 *)

BeginPackage["PlanGeom`"];
Needs["DiscreteMath`ComputationalGeometry`"];
Needs["CombinatoricaModified`"];
Options[ConvexHull] = {AllPoints->False};

(* Eps *)
TolerancePG=10^(-9);

(*
PointQ     [x_] := 
AlignedQ   [x_] :=
LineQ      [x_] :=
SegmentQ   [x_] :=
PolygonQ   [x_] := at least three points, counterclockwise, simple
ProxGraphQ [x_] :=
*)

AlignedQ     ::usage = "AlignedQ[pointset] -> {True, False}";

InPolygon    ::usage = "InPolygon[point, polygon] -> {True, False}";
ConvexHullPG ::usage = "ConvexHullPG[pointset] -> {polygon}";
DiameterPG   ::usage = "DiameterPG[polygon] -> {positive real}";
Area         ::usage = "Area[polygon] -> {positive real}";
FurthestPoint::usage = "FurthestPoint[point, pointset] -> {furthestpoint, distance}. Possibly not unique.";
NearestPoint ::usage = "NearestPoint[point, pointset] -> {nearestpoint, distance}. Possibly not unique.";

BisectorTwoPts        ::usage = "BisectorTwoPts[p1, p2] -> {line}. Warning: p1 = p2. p1 lies in positive halfplane.";
LineThroughTwoPts     ::usage = "LineThroughTwoPts[p1, p2] -> {line}. Warning: p1 = p2";
IntersectTwoLines     ::usage = "IntersectTwoLines[line1, line2]  -> {point}.  Warning: line1 parallel to line2";
CircleThroughThreePts ::usage = "CircleThroughThreePts[p1, p2, p3] -> {center, radius}. Warning: p1,p2,p3 aligned";
CircleThroughThreePts ::"aligned" = "`1`,`2` and `3` aligned."
CircumCircle          ::usage = "CircumCircle[polygon] -> {circumcenter, circumradius}";
InCircle              ::usage = "InCircle[polygon] -> {incenter, inradius}";
PolygonToHalfPlanes   ::usage = "PolygonToHalfPlanes[polygon] -> {halfplanes}. Halfplane constraints in counterclockwise order";
HalfPlanesToPolygon   ::usage = "HalfPlanesToPolygon[halfplanes] -> {polygon}";

OrthProjOnLine         ::usage = "OrthProjOnLine[point, line]   -> {closestpoint, distance}";
OrthProjOnSegment      ::usage = "OrthProjOnSegment[point, segment]  -> {closestpoint, distance}";
OrthProjOnBoundPolygon ::usage = "OrthProjOnBoundPolygon[point, polygon]  -> {closestpoint, distance}";
OrthProjOnPolygon      ::usage = "OrthProjOnPolygon[point, polygon]  -> {closestpoint, distance}";

IntersectSegBall          ::usage = "IntersectSegBall [segment,ball]-> segment";
IntersectSegBallBoundary  ::usage = "IntersectSegSphere[segment,ball] -> {pointset}. Returns points from left to right extremes";
IntersectPolygonBallBoundary  ::usage = "IntersectPolygonBallBoundary[polygon,ball] -> {pointset,path}. Returns pointset and path indicating whether points are connected by a straight line or an arc";

VoronoiDiagramPG              ::usage = "Same as in ComputationalGeometry package.";
VoronoiPartition              ::usage = "VoronoiPartition[point set] -> partition.";
BoundedVoronoiPartition       ::usage = "BoundedVoronoiPartition[pointset, polygon] -> partition";
BoundedVoronoiPartitionFaulty ::usage = "BoundedVoronoiPartitionFaulty[pointset,polygon,faultypoints,faultyadjacencylist] -> covering";
DelaunayTriangulationPG       ::usage = "DelaunayTriangulationPG[pointset] -> adjacency list";
DelaunayTriangulationFaulty   ::usage = "DelaunayTriangulationFaulty[pointset,faultypoints,faultyadjacencylist] -> adjacency list";

CompleteProxGraph           ::usage="CompleteProxGraph[pointset] -> graph";
Delaunay                    ::usage="Delaunay[pointset] -> graph";
ProxGraphGabriel            ::usage="ProxGraphGabriel[pointset] -> graph";
ProxGraphRelNhb             ::usage="ProxGraphRelNhb[pointset] -> graph";
EMST                        ::usage="EMST[pointset] -> graph";
ProxGraphDisk               ::usage="ProxGraphDisk[pointset,radius] -> graph";
LimRangeDelaunay            ::usage="LimRangeDelaunay[pointset,radius] -> graph";
LimRangeDelaunayFaulty      ::usage="LimRangeDelaunayFaulty[pointset,radius,faultypoints,faultyadjacencylist] -> directed graph";

IntersectProxGraph        ::usage = "Same as in Combinatorica package with GraphIntersection.";

RotateShapePG    ::usage = "RotateShapePG[shape, theta] rotates a shape by an angle theta. A shape is a set of graphics primitives.";
TranslateShapePG ::usage = "TranslateShapePG[shape, vec] translates a shape by a vector vec. A shape is a set of graphics primitives.";

SmallestCircleThroughTwoPts  ::usage

Begin["`Private`"];

Vers[p_]     := If[Norm[p]>TolerancePG, p/Norm[p], p];
UnsortedUnion[x_] := Reap[Sow[1, x], _, #1 &][[2]];

(* ---------------------------------- DistPointLine ------------------------------- *)
(* Computes distance between a point and a line: returns point on line and distance *)
DistPointLine[p1_,p2_,q_]:= Module[
    {qx=q[[1]], qy=q[[2]], p1x=p1[[1]], p1y=p1[[2]], p2x=p2[[1]], p2y=p2[[2]], qonline,dq,a},
    qonline = a p1 + (1-a) p2  /. a -> ((p2x - p1x)*(p2x - qx) + (p2y - p1y)*(p2y - qy))/((p1x - p2x)^2 + (p1y - p2y)^2);
    dq = Norm[q-qonline];
    Return[{qonline,dq}];
];

(* ------------------------------- AlignedQ -------------------------------- *)
(* Subroutine to check if points in pointset are aligned *)

AlignedQ[pointset_]:= 
  Module[{ii=3,Align=True,point,dist},
         While[Align==True && (ii <=Length[pointset]),
               dista = DistPointLine[pointset[[1]],pointset[[2]],pointset[[ii]]];
               If[dista[[2]]>TolerancePG,Align=False];
               ii = ii+1;
         ];
         Return[Align];
  ];

If[MatrixRank[pointset,Tolerance->10 * TolerancePG]==1,True,False];

(* ------------------------------- InPolygon -------------------------------- *)
(* Subroutine to check if a certain point belongs to a (closed, convex) polygon.
   
   point    : coordinates of point
   polygon  : vertices of bounding polygon, such that they are in counter clock-wise order.

   For the point to be "counter clock-wise inside" the segment
         (y -y1)   (x -x1)
         ------- - ------- >=0
         (y2-y1)   (x2-x1)
*)

InPolygon [point_, polygon_] := Module[ {inside=True,ii,val},
  Do[
    val=(point[[2]]-polygon[[ii,2]])(polygon[[ii+1,1]]-polygon[[ii,1]])
       -(point[[1]]-polygon[[ii,1]])(polygon[[ii+1,2]]-polygon[[ii,2]]);
    If[val<0, inside=False];
  ,{ii,1,Length[polygon]-1}];
  (* For the last edge *)
  ii = Length[polygon];
  val=(point[[2]]-polygon[[ii,2]])(polygon[[1,1]]-polygon[[ii,1]])
     -(point[[1]]-polygon[[ii,1]])(polygon[[1,2]]-polygon[[ii,2]]);
  If[val<0, inside=False];
  Return[inside];
]

(* --------------------------- ConvexHullPG --------------------------- *)
(* Subroutine to check if a certain point is within the given convex polygon.
   Invokes ConvexHull from ComputationalGeometry.m Package *)
ConvexHullPG[S_] := S[[ ConvexHull[S] ]];

(* --------------------------- DiameterPG --------------------------- *)
(* Computes the diameter of a polygon *)
DiameterPG[S_] := Max[ Table[ Table[ Norm[ S[[i]]-S[[j]] ], {i,1,j}], {j,1,Length[S]} ] ];

(* --------------------------- Area --------------------------- *)
(* Computes the area of a polygon *)
Area[S_] := Module[{NS=Length[S]},  Return[ Sum[  S[[k,1]] S[[Mod[k+1,NS,1], 2]] - S[[Mod[k+1,NS,1],1]] S[[k,2]]   ,{k,1,NS}]/2];];
(* add the PolygonQ check !*)

(* --------------------------- BisectorTwoPts -------------------------- *)
(* Computes bisector line of two points. Warning when p1 = p2. p1 belongs to positive halfplane. *)
BisectorTwoPts[p1_,p2_] :=  Module[{dist=Sqrt[(p2[[1]]-p1[[1]])^2+(p2[[2]]-p1[[2]])^2]},
 If[dist<TolerancePG, 
   Print["Warning in BisectorTwoPts: No unique bisector of same point."]; Return[{}]; , 
   {p1[[1]]-p2[[1]], p1[[2]]-p2[[2]], (p2[[1]]^2-p1[[1]]^2+p2[[2]]^2-p1[[2]]^2)/2}/dist ]
];

(* --------------------------- LineThroughTwoPts -------------------------- *)
(* Computes lines through two points. Warning when p1 = p2 *)
(* The line is expressed as a triple {a,b,c} corresponding to the equation a x + b y + c ==0 *)
LineThroughTwoPts[p1_,p2_]:=Module[{a,b,c,x1=p1[[1]],y1=p1[[2]],x2=p2[[1]],y2=p2[[2]]},
 If[ Norm[p1-p2]< TolerancePG, Print["Warning in LineThroughTwoPts: No unique line through same point."];];
 If[ Abs[x2*y1-x1*y2]>TolerancePG,  {a,b}={y2-y1,x1-x2}/(x2*y1-x1*y2); c=1; ,  
  c=0;
  If[ Abs[x1]>TolerancePG, a=-y1/x1; b=1; ,
   If[ Abs[x2]>TolerancePG, a=-y2/x2; b=1; ,
     If[ Abs[y1]>TolerancePG, b=-x1/y1; a=1;,
      If[ Abs[y2]>TolerancePG, b=-x2/y2; a=1;]]]]];
 Return[ {a,b,c}/Sqrt[a^2+b^2] ]; ];

(* --------------------------- IntersectTwoLines -------------------------- *)
(* Computes intersection of two lines. Gives warning when lines are parallel. *)
IntersectTwoLines[line1_,line2_]:= Module[ {a1=line1[[1]],b1=line1[[2]],c1=line1[[3]],
  a2=line2[[1]],b2=line2[[2]],c2=line2[[3]], det},
 det=a2*b1-a1*b2;
 If[ Abs[det]>TolerancePG, Return[ {b2*c1-b1*c2, a1*c2-a2*c1}/det]]; (*point*)
 Print["Warning in IntersectTwoLines: No unique point is intersection of parallel lines."];
 If[ Abs[a1*c2-c1*a2]>TolerancePG, Return[{}], Return[line1] ]; (* either nothing, or entire line *)
]


(* ------------------------- CircleThroughThreePts ----------------------------------- *)
(* circle through {p1, p2, p3} -> {center, radius}. warning: p1,p2,p3 aligned *)
(* Circlethree: provides angle needed for MinEnclCircle routine *)

CircleThroughThreePts[p1_,p2_,p3_] :=Module[{rad,cen,an,det=Det[{p1-p2,p1-p3}]},
 If[Abs[det]<TolerancePG,
    Message[CircleThroughThreePts ::"aligned",p1,p2,p3]; Return[{}];];
 {rad,cen,an} = CircleThree[p1,p2,p3];
 Return[{cen,rad}]; ]

CircleThree[p1_,p2_,p3_] :=Module[{radius, center, Ac, bc, angle},
  If[ (Norm[p1-p2]<TolerancePG) || (Norm[p2-p3]<TolerancePG) || (Norm[p1-p3]<TolerancePG),
    radius = (Norm[p1-p2]+Norm[p2-p3]+Norm[p1-p3]) .25;
    center = .5 If[ (Norm[p1-p2]<TolerancePG), (p2+p3), (p1+p2)];
    ,    
    Ac = 2 {{ p1[[1]] - p2[[1]], p1[[2]] - p2[[2]] },
            { p1[[1]] - p3[[1]], p1[[2]] - p3[[2]] }};
    bc = { p1[[1]]^2 - p2[[1]]^2 + p1[[2]]^2 - p2[[2]]^2,
           p1[[1]]^2 - p3[[1]]^2 + p1[[2]]^2 - p3[[2]]^2 };
    center = Inverse[Ac] . bc;
    radius = Norm[center-p1];
  ];
  angle = Re[ArcCos[ -(p2-p1).(p3-p2) / (Norm[p2-p1] Norm[p3-p2]) ]];
 Return[{radius,center,angle}];
];

(* ----------------------- CircumCircle ----------------------- *)
(* Subroutine to find CircumCircle of set of points on a 2D plane.
   Implementation based on algorithm by Skyum, 1991.
  input: 
   pointset = {p1,p2,...,pn}      : coordinates of points to be
                                    enclosed by the circle
  outputs:
   center                         : center of the circle
   radius                         : radius of the circle
*)

CircumCircle[pointset_]:=Module[
  {MySet,lengthS,center,radius,finish,MaxIndex,MaxRadius,MaxAngle,MaxCenter,p,pnext,pprev,tmpradius,tmpcenter,tmpangle,i,
                                    pointsetOrdered,SmallestPoint,LargestPoint,circle},  
  (* obtain the convex hull *)
  MySet = ConvexHull[pointset];  lengthS = Length[MySet];
  If[lengthS == 1,    
     center = pointset[[ MySet[[1]] ]]; 
     radius = 0.0;
     ,
     If[AlignedQ[pointset],     
        pointsetOrdered = Sort[pointset];
        SmallestPoint = pointsetOrdered[[1]];
        LargestPoint = pointsetOrdered[[Length[pointset]]];
        Print["pointset"];
        circle = SmallestCircleThroughTwoPts[SmallestPoint,LargestPoint];
        Return[circle];
     ];
     finish = False;
     While[finish==False,        
           MaxIndex  = MaxRadius = MaxAngle  = MaxCenter = 0;
           Do[
              p     = pointset[[ MySet[[i]] ]];
              pnext = pointset[[ MySet[[ Mod[i-1,lengthS, 1] ]] ]];
              pprev = pointset[[ MySet[[ Mod[i+1,lengthS, 1] ]] ]];
           {tmpradius,tmpcenter,tmpangle} = CircleThree[pprev,p,pnext];
           If[ (tmpradius>MaxRadius+TolerancePG) || ( Abs[tmpradius-MaxRadius]<TolerancePG && tmpangle>MaxAngle) , 
               MaxIndex = i;
           MaxRadius = tmpradius;
           MaxAngle  = tmpangle;
           MaxCenter = tmpcenter;
         ];
         ,{i, lengthS, 1, -1}];         
         If[ MaxAngle <= Pi/2,
           finish = True;
           radius = MaxRadius;
           center = MaxCenter;
           ,      
           lengthS = lengthS - 1;   
           MySet     = Delete[MySet, MaxIndex];    ];
    ];
  ]; 
  Return[{center, radius}];
]

(* Subroutine to find InCircle of set of points on a 2D plane.
  input:    polygon = {p1,p2,...,pn}      : coordinates of polygon enclosing the circle
  outputs:  center, radius                 : center & radius of the circle
*)
InCircle[polygon_]:=Module[ {n=Length[polygon],halfplanes,mm={},bb={},radius,xc,yc,ii, tmp},
 tmp =  2 Abs[Min[polygon,0]]; 
 halfplanes = PolygonToHalfPlanes[N[polygon + tmp]];
 Do[ 
 mm = Append[mm, Flatten[{-1, halfplanes[[ii,{1,2}]]}]  ];  
 bb = Append[bb, -halfplanes[[ii,3]]]; 
 ,{ii,1,n}];
 {radius,xc,yc} =  LinearProgramming[{-1,0,0}, mm,bb];
 Return[{{xc,yc}-tmp,radius}];
]


(* Explanation: LinearProgramming[c, m, b] finds the vector x which
   minimizes the quantity c.x subject to the constraints m.x >= b and x >=0.

   find largest incribed circle:
   x={radius,xc,yc}
   assuming the polygon is given by    ai x + bi y +ci =>0    for   ai^2+bi^2=1
   then constraints:
   ai x + bi y +ci => radius    iff   {-1, ai, bi} .{radius,xc,yc} >= -ci
   LinearProgramming[{-1,0,0}, mm,bb]     
  (CAREFUL: move all polygon vertices in positive x and y; then subtract from xc yc)
*)

(* Sample run for the InCircle 
 polygon = {{0, 0}, {2, 0}, {2, 1}, {1,3}, {0, 1}};
 {center,radius}=InCircle[polygon];
 Show[ Graphics[  {Polygon[polygon], Blue, Disk[center,radius] } , AspectRatio->1, PlotRange->{{-.5,3},{-.5,3}} ] ]
*)


(* --------------------------- PolygonToHalfPlanes -------------------------- *)
PolygonToHalfPlanes[polygon_]:=Module[{n=Length[polygon],ii,a,b,c},
Table[ {a,b,c} = LineThroughTwoPts[ polygon[[ii]], polygon[[ Mod[ii+1,n,1] ]] ];
   If[{a,b}.polygon[[ Mod[ii+2,n,1] ]] + c <0, {a,b,c}=-{a,b,c};];
    {a,b,c}/Sqrt[a^2+b^2] ,{ii,1,n}] ];

(* --------------------------- HalfPlanesToPolygon -------------------------- *)
HalfPlanesToPolygon[halfplanes_] :=  Module[{ii,len=Length[halfplanes]},
  Table[   {a1,b1,c1}=halfplanes[[ii]];  {a2,b2,c2}=halfplanes[[Mod[ii+1,len,1] ]];
  {(b1*c2-b2*c1)/(a1*b2-a2*b1), (a2*c1-a1*c2)/(a1*b2-a2*b1)}, {ii,1,len}] ];

(* --------------- FurthestPoint and NearestPoint ----------------------- *)
(* Subroutine to find FurthestPoint of set of points to a specific point
  **input: 
   pointset = {p1,p2,...,pn}      : coordinates of polygon vertexes
   point    = q                   : coordinates point 
  **outputs:
   furthestpoint, distance
*)

FurthestPoint[q_,pointset_] := Module[{maxnorm=0,maxindex,ii},
   For[ii=1,ii<= Length[pointset],ii=ii+1,
   If[ ((q-pointset[[ii]]).(q-pointset[[ii]]))>=maxnorm, 
           maxnorm=(q-pointset[[ii]]).(q-pointset[[ii]]); maxindex=ii; ];];
   Return[{pointset[[maxindex]], Sqrt[maxnorm]}];]

NearestPoint[q_,pointset_] := Module[{minnorm=+Infinity,minindex,ii},
   For[ii=1,ii<= Length[pointset],ii=ii+1,
   If[ ((q-pointset[[ii]]).(q-pointset[[ii]]))<=minnorm, 
           minnorm=(q-pointset[[ii]]).(q-pointset[[ii]]); minindex=ii; ];];
   Return[{pointset[[minindex]], Sqrt[minnorm]}];]

(* ---------------------- OrthProjOnLine    ---------------------------- *)
(* ---------------------- OrthProjOnSegment ---------------------------- *)
(* ---------------------- OrthProjOnPolygon ---------------------------- *)
(* distance between a point and an object; maps [point, object]  into {closestpoint, distance} *)

OrthProjOnLine[q_,line_]:= Module[{a,b,c,dq,qonline},
  {a,b,c} = line/Norm[line[[{1,2}]]];
  qonline = {-a c -q[[2]] b a + b^2 q[[1]],  -b c -q[[1]] b a + a^2 q[[2]] };
  Return[ {qonline, Norm[q-qonline]} ];
];

OrthProjOnSegment[q_,S_]:= Module[
  {qx=q[[1]], qy=q[[2]], p1x=S[[1]][[1]], p1y=S[[1]][[2]], p2x=S[[2]][[1]], p2y=S[[2]][[2]], qonline,d1,d2,dq,a},
  a = ((p2x - p1x)*(p2x - qx) + (p2y - p1y)*(p2y - qy))/((p1x - p2x)^2 + (p1y - p2y)^2) ;
  qonline = a S[[1]] + (1-a) S[[2]]  ;
  d1=Norm[q-S[[1]]];   d2=Norm[q-S[[2]]];  dq=Norm[q-qonline];  
  Return[  If[ a >= 0 && a <= 1 && dq<=Min[d1,d2], {qonline,dq}, If[d1<=Min[dq,d2], {S[[1]],d1}, {S[[2]],d2}]] ];
];

OrthProjOnBoundPolygon[q_, polygon_]:= Module[
  {minnorm=+Infinity,minpoint={0,0},ii,dii,pii},
   For[ii=1,ii<=Length[polygon],ii=ii+1,
    {pii,dii} = OrthProjOnSegment[q, {polygon[[ii]], polygon[[Mod[ii+1,Length[polygon],1]]]}]; 
    If[ (dii<minnorm), minnorm=dii; minpoint = pii; ];
   ];
   Return[{minpoint, minnorm}];
];

OrthProjOnPolygon[q_, polygon_]:= If[ InPolygon[q,polygon], {q,0},  OrthProjOnBoundPolygon[q, polygon]];

VoronoiPartition[pointset_] := Module[{diagvert,diagval,ii,n=Length[pointset],rr,ss,IdentityDistinctVertices,IdentityVertex},
   {diagvert, diagval} = VoronoiDiagram[pointset];
   If[n != Length[diagval],
      IdentityDistinctVertices = diagval[[Table[rr,{rr,1,Length[diagval]}],1]];
      Do[
         If[Cases[IdentityDistinctVertices,ss] == {},
            IdentityVertex = Select[IdentityDistinctVertices,Norm[pointset[[#]]-pointset[[ss]]]<TolerancePG &];
            diagval = Append[diagval,{ss,diagval[[IdentityVertex[[1]],2]]}];
            ];
      ,{ss,1,n}];
      diagval = Sort[diagval];
   ];    
   Table[ diagvert[[diagval[[ii,2]]]] ,{ii,1,Length[pointset]}]
];

BoundedVoronoiPartition[pointset_,polygon_]:= Module[{HalfPlanesPolygon,DelTriang,AllHalfPlanes,aa,
                             n=Length[pointset],ndist=Length[Union[pointset]],ss,rr,ii,kk,IdentityVertex,IdentityDistinctVertices},
   If[ndist == 1,Return[Table[polygon,{ii,1,n}]]];
   If[ndist == 2,
      DelTriang = {{1,{Length[Split[Union[pointset]][[1]]]+1}},{Length[Split[Union[pointset]][[1]]]+1,{1}}};,
      DelTriang = DelaunayTriangulationPG[pointset];
      (* Earlier implementation with DelaunayTriangulation *)
      (*
      DelTriang = DelaunayTriangulation[pointset];
       Table[DelTriang[[aa,2]] = UnsortedUnion[DelTriang[[aa,2]]],{aa,1,ndist}];
       *)
   ];
   (* For earlier implementation with DelaunayTriangulation *)
   (*
   If[n!=ndist,
      IdentityDistinctVertices = DelTriang[[Table[rr,{rr,1,Length[DelTriang]}],1]];
      Do[
         If[Cases[IdentityDistinctVertices,ss] == {},
            IdentityVertex = Select[IdentityDistinctVertices,Norm[pointset[[#]]-pointset[[ss]]]<TolerancePG &];
            DelTriang = Append[DelTriang,{ss,DelTriang[[Length[Union[pointset[[Table[kk,{kk,1,IdentityVertex[[1]]}]]]]],2]]}];
         ];
      ,{ss,1,n}];
      DelTriang = Sort[DelTriang];
   ];
   *)
   HalfPlanesPolygon = PolygonToHalfPlanes[polygon];
   AllHalfPlanes = BoundedVoronoiPlanesPG[pointset, DelTriang, HalfPlanesPolygon]; 
   Table[  Reverse[ HalfPlanesToPolygon[ AllHalfPlanes[[kk]] ] ] ,{kk,1,n}] 
]

BoundedVoronoiPlanesPG[pointset_,Triangulation_,Bounds_] := 
Module[{n=Length[pointset],nghbrs,VoronoiHalfplanes,ii},
 Table[
  (* For earlier implementation with DelaunayTriangulation *)
  (* nghbrs = pointset [[  Triangulation[[ii,2]] ]]; *)
  nghbrs = pointset [[Triangulation[[ii]]]];
  VoronoiHalfplanes =  Table[BisectorTwoPts[pointset[[ii]], nghbrs[[jj]] ], {jj,Length[nghbrs]}];
  (* add halfplanes due to bounded domain, check no duplicates, order counterclockwise ArcTan *)
  VoronoiHalfplanes = Sort[ Union[VoronoiHalfplanes,Bounds],  ( ArcTan[#2[[1]],#2[[2]]] < ArcTan[#1[[1]],#1[[2]]] )& ];
  (* finally remove duplicates *)
  DropRedundantHalfPlanes[VoronoiHalfplanes]
 ,{ii,1,n}] ];

DropRedundantHalfPlanes[someplanes_]:= Module[{ii,NumSteps,lenP,planes},
 planes=someplanes; ii=1; lenP=Length[planes]; NumSteps=0;
 While[True,  (*Print["Step ",ii,"; length ",lenP]; Print[InputForm[planes]];*)
  If[ 
   IsRedundant[ planes[[Mod[ii,lenP,1]]], planes[[Mod[ii+1,lenP,1]]], planes[[Mod[ii+2,lenP,1]]] ]  
   ,
   planes = Drop[planes, {Mod[ii+1,lenP,1]}]; NumSteps=0; lenP=lenP-1; ,
   ii=ii+1; NumSteps=NumSteps+1;  
  ];
  If[ NumSteps == lenP+1, Return[planes];];
];
]

IsRedundant[plane1_,plane2_,plane3_]:= Module[
 {a1,b1,c1,a2,b2,c2,a3,b3,c3,det},
 {a1,b1,c1}=plane1;{a2,b2,c2}=plane2;{a3,b3,c3}=plane3;
   (* if 1st and 3rd line form angle greater than Pi, then keep 2nd*)
 If[ Mod[2Pi+(ArcTan[a1,b1]-ArcTan[a3,b3]),2Pi]>Pi, Return[False];]; 
 det=a1 b3 - a3 b1;
   (* if 1st and 3rd line are parallel, then keep 2nd*)
 If[ Abs[det] < TolerancePG,  Return[False]; ];
   (* compute intersection p={(b1*c3-b3*c1)/det,(a3*c1-a1*c3)/det} between 1st and 3rd plane *)
   (* if p is feasible according to 2nd line, then discard 2nd line *)
  If[ (a2 (b1*c3-b3*c1)/det + b2 (a3*c1-a1*c3)/det + c2  >= 0),  Return[True];, Return[False];]; 
];

BoundedVoronoiPartitionFaulty[pointset_,polygon_,faultypoints_,FaultyAdjacencyList_]:= Module[{HalfPlanesPolygon,DelTriang,
         AllHalfPlanes,aa,n=Length[pointset],ndist=Length[Union[pointset]],ss,rr,ii,kk,IdentityVertex,IdentityDistinctVertices},
   If[ndist == 1,Return[Table[polygon,{ii,1,n}]]];
   If[ndist == 2,
      DelTriang = {{1,{Length[Split[Union[pointset]][[1]]]+1}},{Length[Split[Union[pointset]][[1]]]+1,{1}}};,
      DelTriang = DelaunayTriangulationFaulty[pointset,faultypoints,FaultyAdjacencyList];
   ];
   HalfPlanesPolygon = PolygonToHalfPlanes[polygon];
   AllHalfPlanes = BoundedVoronoiPlanesPG[pointset, DelTriang, HalfPlanesPolygon];
   Return[Table[Reverse[ HalfPlanesToPolygon[ AllHalfPlanes[[kk]] ] ] ,{kk,1,n}]];
]

(* -------------------------------- IntersectSegBallBoundary ------------------------------------------ *)
(* Computes intersection between segment and sphere *)
(* Returns points inside the segment in order from left vertex to right vertex, otherwise empty *)

IntersectSegBallBoundary[segment_,sphere_]:=
   Module[{qinseg={},qonline1,qonline2,radicand,a,b,c,dist1,dist2,p1=segment[[1]],p2=segment[[2]],center=sphere[[1]],radius=sphere[[2]]},
    {a,b,c} = LineThroughTwoPts[p1,p2]; (* output of LineThroughTwoPts must be normalized *)
    radicand = radius^2 - (c+a center[[1]] + b center[[2]])^2;
    If[radicand >=0,
       qonline1 = {b^2 center[[1]]-a c -a b center[[2]] - b Sqrt[radicand],-b a center[[1]] - b c + a^2 center[[2]] + a Sqrt[radicand]};
       qonline2 = {b^2 center[[1]]-a c -a b center[[2]] + b Sqrt[radicand],-b a center[[1]] - b c + a^2 center[[2]] - a Sqrt[radicand]};
       dist1 = Max[Norm[qonline1-p1],Norm[qonline1-p2]];
       dist2 = Max[Norm[qonline2-p1],Norm[qonline2-p2]];
       If[dist1 <= Norm[p1-p2],
          qinseg = Append[qinseg,qonline1];
       ];
       If[(qonline1 != qonline2)&&(dist2 <= Norm[p1-p2]),
          If[Norm[qonline1-p1]<Norm[qonline2-p1],
             qinseg = Append[qinseg,qonline2];
             ,
             qinseg = Prepend[qinseg,qonline2];
          ];
       ];
    ];
    Return[qinseg];
];

IntersectSegBall[segment_,ball_]:=
   Module[{qonline1,qonline2,radicand,a,b,c,dist1,dist2,p1=segment[[1]],p2=segment[[2]],center=ball[[1]],radius=ball[[2]],intersection={}},
       If[Norm[p1-center]<=radius && Norm[p2-center]<=radius,
          intersection = segment;
          ,
          If[Norm[p2-center]<=radius,{p1,p2}={p2,p1};];
          {a,b,c} = LineThroughTwoPts[p1,p2]; (* output of LineThroughTwoPts must be normalized *)
          radicand = radius^2 - (c+a center[[1]] + b center[[2]])^2;
          If[radicand >=0,
             qonline1 = {b^2 center[[1]]-a c -a b center[[2]] - b Sqrt[radicand],-b a center[[1]] - b c + a^2 center[[2]] + a Sqrt[radicand]};
             qonline2 = {b^2 center[[1]]-a c -a b center[[2]] + b Sqrt[radicand],-b a center[[1]] - b c + a^2 center[[2]] - a Sqrt[radicand]};
             dist1 = Max[Norm[qonline1-p1],Norm[qonline1-p2]];
             dist2 = Max[Norm[qonline2-p1],Norm[qonline2-p2]];
             If[dist1<= Norm[p1-p2] && dist2<= Norm[p1-p2],
                intersection = {qonline1,qonline2}
                ,
                If[dist1 <=Norm[p1-p2],intersection = {p1,qonline1},If[dist2 <=Norm[p1-p2],intersection = {p1,qonline2}]];
             ];
          ];
       ];
    Return[intersection];
];


(* --------------------------- IntersectPolygonBallBoundary ------------------------ *)
(* Computes intersection between polygon and sphere *)
(* Returns points bounding the intersection, and information about whether the 
        points are connected by a straight or an arc segment *)

IntersectPolygonBallBoundary[polygon_,sphere_] :=
   Module[{LeftVertex,RightVertex,qinseg,qintersec={},pathintersec={},n=Length[polygon],criterium,center=sphere[[1]],radius=sphere[[2]]},
      (* The information about straight/arc segment is saved in pathintersec with 0/1*) 
      Do[
         LeftVertex = polygon[[ss]];
         RightVertex = polygon[[Mod[ss+1,n,1]]];
         qinseg = IntersectSegBallBoundary[{LeftVertex,RightVertex},sphere];
         If[qinseg != {},
            qintersec = Join[qintersec,qinseg];
            If[Length[qinseg] == 2, 
               pathintersec = Join[pathintersec,{0,1}];
               ,
               criterium = (RightVertex - LeftVertex).(center - qinseg[[1]]);
               (* The intersection goes along the polygon if criterium is strictly positive, otherwise along the sphere *)
               If[criterium>0,
                  qintersec = Join[qintersec,{RightVertex}];
                  pathintersec = Join[pathintersec,{0,0}];
                  ,
                  pathintersec = Join[pathintersec,{1}];                
               ];
            ];
            ,
            If[Norm[center-LeftVertex]<=radius,
               qintersec = Join[qintersec,{RightVertex}];
               pathintersec = Join[pathintersec,{0}];
               ];
         ];          
      ,{ss,1,n}];
      If[(qintersec == {})&&InPolygon[center,polygon],
         qintersec = {center + radius {1,0},center + radius {-1,0}};
         pathintersec = {1,1};          
      ];
      Return[{qintersec,pathintersec}];
   ];


(* ------------------ RotateShapePG and TranslateShapePG -------------*)
(* rotate and translate shapes composed of graphics primitives *)
RotateShapePG[shape_, th_ ] :=
    Block[{R={{Cos[th],-Sin[th]},{Sin[th],Cos[th]}} },	
	shape /. { poly:Polygon[_] :> Map[(R . #)&, poly, {2}],
	           line:Line[_]    :> Map[(R . #)&, line, {2}],
                   point:Point[_]  :> Map[(R . #)&, point,{1}] }    ];
			 
TranslateShapePG[shape_, vec_] :=
    Block[{tvec = N[vec]},
	shape /. { poly:Polygon[_] :> Map[(tvec + #)&, poly, {2}],
		   line:Line[_]    :> Map[(tvec + #)&, line, {2}],
		   point:Point[_]  :> Map[(tvec + #)&, point,{1}] ,
		   Circle[x_,r_]   :> Circle[x+tvec,r] ,
		   Disk[x_,r_]     :> Disk[x+tvec,r]}
    ];



(* ------------------ Proximity Graph Code -------------*)

SmallestCircleThroughTwoPts[p1_,p2_]:={(p1+p2)/2,Norm[p1-p2]/2};

CompleteProxGraph[pointset_]:= Module[{vertices=Partition[pointset,1],edges,n=Length[pointset]},
   edges=KSubsets[Table[kk,{kk,1,n}],2];
   Do[
      edges = ReplacePart[edges,{edges[[ii]],EdgeWeight -> 
              Norm[pointset[[edges[[ii,1]]]]-pointset[[edges[[ii,2]]]]]},ii];
   ,{ii,1,Length[edges]}];
   Return[Graph[edges,vertices]];
];

EMST[pointset_]:= MinimumSpanningTree[CompleteProxGraph[pointset]];

Delaunay[pointset_]:= Module[{vertices=Partition[pointset,1],edges,DelaunayAdjacencyList,n=Length[pointset],
                                          ndist=Length[Union[pointset]],Triang,IdentityDistinctVertices,IdentityVertex,rr,ss,kk,aa},
   If[ndist==1,Return[Graph[{},vertices]]];
   If[ndist==2,
      Triang = {{1,{Length[Split[Union[pointset]][[1]]]+1}},{Length[Split[Union[pointset]][[1]]]+1,{1}}};,
      Triang = DelaunayTriangulation[pointset];
      Table[Triang[[aa,2]] = Union[Triang[[aa,2]]],{aa,1,ndist}];
    ];
   If[n!=ndist,
      IdentityDistinctVertices = Triang[[Table[rr,{rr,1,Length[Triang]}],1]];
      Do[
         If[Cases[IdentityDistinctVertices,ss] == {},
            IdentityVertex = Select[IdentityDistinctVertices,Norm[pointset[[#]]-pointset[[ss]]]<TolerancePG &];
            Triang = Append[Triang,{ss,Triang[[Length[Union[pointset[[Table[kk,{kk,1,IdentityVertex[[1]]}]]]]],2]]}];
            ];
      ,{ss,1,n}];
      Triang = Sort[Triang];
   ];
   DelaunayAdjacencyList = DeleteCases[Triang,_Integer,2];
   edges = FromAdjacencyLists[Flatten[DelaunayAdjacencyList,1]][[1]];
   Return[Graph[edges,vertices]];
];

(* Computation of Gabriel adjacency lists *)
(* By deleting edges from Delaunay adjacency lists *) 
ProxGraphGabriel[pointset_] := Module[
 {n=Length[pointset],ndist=Length[Union[pointset]],GabrielAdjacencyList,vertices=Partition[pointset,1],edges,ii,jj,
                              DelaunayAdjacencyList,spherec,spherer,ElementsToDelete,IdentityDistinctVertices,IdentityVertex,ss,rr,aa},
 If[ndist==1,Return[Graph[{},vertices]]];
 If[ndist==2,
    DelaunayAdjacencyList = {{1,{Length[Split[Union[pointset]][[1]]]+1}},{Length[Split[Union[pointset]][[1]]]+1,{1}}};,
    DelaunayAdjacencyList = DelaunayTriangulation[pointset];
    Table[DelaunayAdjacencyList[[aa,2]] = Union[DelaunayAdjacencyList[[aa,2]]],{aa,1,ndist}];
 ];
 If[n!=ndist,
      IdentityDistinctVertices = DelaunayAdjacencyList[[Table[rr,{rr,1,Length[DelaunayAdjacencyList]}],1]];
      Do[
         If[Cases[IdentityDistinctVertices,ss] == {},
            IdentityVertex = Select[IdentityDistinctVertices,Norm[pointset[[#]]-pointset[[ss]]]<TolerancePG &];
            DelaunayAdjacencyList = Append[DelaunayAdjacencyList,
                {ss,DelaunayAdjacencyList[[Length[Union[pointset[[Table[kk,{kk,1,IdentityVertex[[1]]}]]]]],2]]}];
            ];
      ,{ss,1,n}];
      DelaunayAdjacencyList = Sort[DelaunayAdjacencyList];
   ];
   GabrielAdjacencyList =  DelaunayAdjacencyList;
   Do[
      DelaunayAdjacencyList = ReplacePart[DelaunayAdjacencyList,DelaunayAdjacencyList[[ii,2]],ii];
      GabrielAdjacencyList[[ii]] = DelaunayAdjacencyList[[ii]];
      ElementsToDelete = {};
      Do[
         {spherec,spherer} = SmallestCircleThroughTwoPts[ pointset[[ii]], pointset[[ DelaunayAdjacencyList[[ii,jj]] ]] ];
         If[MemberQ[NonNegative[Table[Norm[spherec - pointset[[rr]]] - spherer + TolerancePG,{rr,1,n}]],False],
            ElementsToDelete = Append[ElementsToDelete,{jj}];
         ];
      ,{jj,1,Length[DelaunayAdjacencyList[[ii]]]}];
      GabrielAdjacencyList[[ii]] = Delete[GabrielAdjacencyList[[ii]],ElementsToDelete];
   ,{ii,1,n}];
   edges = FromAdjacencyLists[GabrielAdjacencyList] [[1]];
   Return[Graph[edges,vertices]];
]

(* Computation of RNG adjacency lists *)
(* By deleting edges from Delaunay adjacency lists *) 
ProxGraphRelNhb[pointset_] := Module[
   {n=Length[pointset],ndist=Length[Union[pointset]],RelNhbAdjacencyList,vertices=Partition[pointset,1],edges,ii,jj,ss,rr,kk,aa,
                              DelaunayAdjacencyList,dist,spherec1,spherec2,ElementsToDelete,IdentityDistinctVertices,IdentityVertex},
   If[ndist==1,Return[Graph[{},vertices]]];
   If[ndist==2,
      DelaunayAdjacencyList = {{1,{Length[Split[Union[pointset]][[1]]]+1}},{Length[Split[Union[pointset]][[1]]]+1,{1}}};,
      DelaunayAdjacencyList = DelaunayTriangulation[pointset];
      Table[DelaunayAdjacencyList[[aa,2]] = Union[DelaunayAdjacencyList[[aa,2]]],{aa,1,ndist}];
   ];
   If[n!=ndist,
      IdentityDistinctVertices = DelaunayAdjacencyList[[Table[rr,{rr,1,Length[DelaunayAdjacencyList]}],1]];
      Do[
         If[Cases[IdentityDistinctVertices,ss] == {},
            IdentityVertex = Select[IdentityDistinctVertices,Norm[pointset[[#]]-pointset[[ss]]]<TolerancePG &];
            DelaunayAdjacencyList = Append[DelaunayAdjacencyList,
                {ss,DelaunayAdjacencyList[[Length[Union[pointset[[Table[kk,{kk,1,IdentityVertex[[1]]}]]]]],2]]}];
            ];
      ,{ss,1,n}];
      DelaunayAdjacencyList = Sort[DelaunayAdjacencyList];
   ];
   RelNhbAdjacencyList = DelaunayAdjacencyList;
   Do[
     DelaunayAdjacencyList = ReplacePart[DelaunayAdjacencyList,DelaunayAdjacencyList[[ii,2]],ii];
     RelNhbAdjacencyList[[ii]] = DelaunayAdjacencyList[[ii]];
     ElementsToDelete = {};
     Do[
        spherec1 = pointset[[ii]];
        spherec2 = pointset[[ DelaunayAdjacencyList[[ii,jj]] ]] ;
        dist = Norm[spherec1 - spherec2];
        Do[ 
           If[(Norm[spherec1-pointset[[rr]]]-dist+TolerancePG<0) && (Norm[spherec2-pointset[[rr]]]-dist+TolerancePG<0)
             ,ElementsToDelete = Append[ElementsToDelete,{jj}]; ];
	,{rr,1,n}];
     ,{jj,1,Length[DelaunayAdjacencyList[[ii]]]}];
   RelNhbAdjacencyList[[ii]] = Delete[RelNhbAdjacencyList[[ii]],ElementsToDelete];
   ,{ii,1,n}];
   edges = FromAdjacencyLists[RelNhbAdjacencyList] [[1]];
   Return[Graph[edges,vertices]];
]

ProxGraphDisk[pointset_,radius_]:= Module[{vertices=Partition[pointset,1],edges,n=Length[pointset],ii,tt,kk},
   edgesCheck = Flatten[NonPositive[Table[Table[If[Norm[ pointset[[tt]]- pointset[[ii]]] < TolerancePG,1,Norm[ pointset[[tt]]- pointset[[ii]]] - radius - TolerancePG],{ii,tt+1,n}],{tt,1,n-1}]]];
   edges = Partition[Delete[KSubsets[Table[kk,{kk,1,n}],2],Position[Flatten[edgesCheck],False]],1];
   Return[Graph[edges,vertices]];
];

(* Computes symmetric point of q with respect to line determined by p1 and p2 *)
SymmetricPoint[p1_,p2_,q_]:= Module[{dist},
   dist = OrthProjOnLine[q,LineThroughTwoPts[1.p1,1.p2]];
   Return[ 2. dist[[1]] - q ];
];

LimRangeDelaunay[pointset_,radius_]:= 
 Module[{ii,vertices=Partition[pointset,1],edges,n=Length[pointset], RLDAdjList, 
         BigPoly,minxy,maxxy,BVP,VI,PI,NumberVI,psubi,CandidateVertex,Differ,PositionCandidateVertex},
  RLDAdjList = Table[{},{n}];
  minxy = Min[Flatten[pointset]] - 2 radius;
  maxxy = Max[Flatten[pointset]] + 2 radius;
  BigPoly = {{minxy,minxy}, {maxxy,minxy}, {maxxy,maxxy}, {minxy,maxxy}};
  BVP = BoundedVoronoiPartition[pointset, BigPoly];
  Do[
     psubi = pointset[[ii]];
     {VI,PI} = IntersectPolygonBallBoundary[ BVP[[ii]], {psubi,radius/2}];
     NumberVI = Length[VI];
     Do[      
        If[PI[[ll]]==0,
           CandidateVertex = SymmetricPoint[VI[[ll]],VI[[Mod[ll+1,NumberVI,1]]],psubi];
           Differ = Table[Norm[pointset[[ww]] - CandidateVertex],{ww,1,n}];
           PositionCandidateVertex = Flatten[Position[Differ,_?(# < TolerancePG &)]];
           RLDAdjList[[ii]] = Append[RLDAdjList[[ii]], PositionCandidateVertex[[1]]];
        ];
     ,{ll,1,NumberVI}];
  ,{ii,1,n}];
  edges = FromAdjacencyLists[RLDAdjList] [[1]];
  Return[Graph[edges,vertices]];
];

LimRangeDelaunayFaulty[pointset_,radius_,faultypoints_,FaultyAdjacencyList_]:= 
  Module[{ii,vertices=Partition[pointset,1],edges,n=Length[pointset], RLDAdjList,
         BigPoly,minxy,maxxy,BVP,VI,PI,NumberVI,psubi,CandidateVertex,Differ,PositionCandidateVertex},
  RLDAdjList = Table[{},{n}];
  minxy = Min[Flatten[pointset]] - 2 radius;
  maxxy = Max[Flatten[pointset]] + 2 radius;
  BigPoly = {{minxy,minxy}, {maxxy,minxy}, {maxxy,maxxy}, {minxy,maxxy}};
  BVP = BoundedVoronoiPartitionFaulty[pointset, BigPoly,faultypoints,FaultyAdjacencyList];
  Do[
     psubi = pointset[[ii]];
     {VI,PI} = IntersectPolygonBallBoundary[ BVP[[ii]], {psubi,radius/2}];
     NumberVI = Length[VI];
     Do[      
        If[PI[[ll]]==0,
           (* This is not fully correct if two points are exactly distance radius apart *)
           CandidateVertex = SymmetricPoint[VI[[ll]],VI[[Mod[ll+1,NumberVI,1]]],psubi];
           Differ = Table[Norm[pointset[[ww]] - CandidateVertex],{ww,1,n}];
           PositionCandidateVertex = Flatten[Position[Differ,_?(# < TolerancePG &)]];
           RLDAdjList[[ii]] = Append[RLDAdjList[[ii]], PositionCandidateVertex[[1]]];
        ];
     ,{ll,1,NumberVI}];
  ,{ii,1,n}];
  edges = FromAdjacencyLists[RLDAdjList,Type->Directed] [[1]];
  Return[Graph[edges,vertices,EdgeDirection->True]];
];

DelaunayTriangulationPG[pointset_]:=Module[{n=Length[pointset],subsetsThree,p1,p2,p3,CircletoCheck,spherec,spherer,
        DelaunayEdges={},adjacencylist},
   subsetsThree = KSubsets[Table[kk,{kk,1,n}],3];
   While[subsetsThree!={},
      p1=pointset[[subsetsThree[[1,1]]]]; p2=pointset[[subsetsThree[[1,2]]]]; p3=pointset[[subsetsThree[[1,3]]]];
      CircletoCheck = CircleThroughThreePts[p1,p2,p3];
      If[CircletoCheck != {},
         spherec=CircletoCheck[[1]]; spherer=CircletoCheck[[2]];
         If[MemberQ[NonNegative[Table[Norm[spherec - pointset[[rr]]] - spherer + TolerancePG,{rr,1,n}]],False]==False,
            DelaunayEdges = Join[DelaunayEdges,{{subsetsThree[[1,1]],subsetsThree[[1,2]]},{subsetsThree[[1,1]],subsetsThree[[1,3]]},{subsetsThree[[1,2]],subsetsThree[[1,3]]}}];
         ];
      ];
      subsetsThree=Complement[subsetsThree,{subsetsThree[[1]]}];
   ];
   DelaunayEdges = Partition[Union[DelaunayEdges],1];
   adjacencylist =  ToAdjacencyLists[Graph[DelaunayEdges,Table[{{0,0}},{rr,1,n}]]];
   Do[
      adjacencylist[[ww]] = Sort[adjacencylist[[ww]],
         (ArcTan[pointset[[#2]][[1]]-pointset[[ww,1]],pointset[[#2]][[2]]-pointset[[ww,2]]]>ArcTan[pointset[[#1]][[1]]-pointset[[ww,1]],pointset[[#1]][[2]]-pointset[[ww,2]]])&];
   ,{ww,1,n}];
   Return[adjacencylist];
];

DelaunayTriangulationFaulty[pointset_,faultypoints_,FaultyAdjacencyList_]:=Module[{n=Length[pointset],subsetsThree,p1,p2,p3,
        CircletoCheck,spherec,kk,spherer,DelaunayEdges={},FaultAgents,ReliAgents,Criteria,ii,jj,qq,
        rr,ss,ww, EdgesToAdd,AddingLink,ToAdd,Deladjacencylist},
   subsetsThree = KSubsets[Table[kk,{kk,1,n}],3];
   While[subsetsThree!={},
      p1=pointset[[subsetsThree[[1,1]]]]; p2=pointset[[subsetsThree[[1,2]]]]; p3=pointset[[subsetsThree[[1,3]]]];
      CircletoCheck = CircleThroughThreePts[p1,p2,p3];
      If[CircletoCheck != {},
         spherec = CircletoCheck[[1]]; spherer=CircletoCheck[[2]];
         Criteria = Position[NonNegative[Table[Norm[spherec - pointset[[rr]]] - spherer + TolerancePG,{rr,1,n}]],False];
         FaultAgents = Intersection[subsetsThree[[1]],faultypoints];
         ReliAgents = Complement[subsetsThree[[1]],faultypoints];
         If[Criteria == {},
            Switch[Length[FaultAgents],
                   0,
                   DelaunayEdges = Join[DelaunayEdges,Flatten[Map[Permutations,KSubsets[subsetsThree[[1]],2]],1]];,
                   1,
                   DelaunayEdges = Join[DelaunayEdges,{{ReliAgents[[1]],ReliAgents[[2]]},{ReliAgents[[2]],ReliAgents[[1]]}}];
                   EdgesToAdd = LinkBetwFaultyReliableAgents[FaultAgents[[1]],FaultyAdjacencyList[[FaultAgents[[1]]]],ReliAgents[[1]]];
                   DelaunayEdges = Join[DelaunayEdges,EdgesToAdd];
                   EdgesToAdd = LinkBetwFaultyReliableAgents[FaultAgents[[1]],FaultyAdjacencyList[[FaultAgents[[1]]]],ReliAgents[[2]]];
                   DelaunayEdges = Join[DelaunayEdges,EdgesToAdd];,
                   2,
                   EdgesToAdd = LinkBetwFaultyAgents[FaultAgents[[1]],FaultAgents[[2]],FaultyAdjacencyList[[FaultAgents[[1]]]],FaultyAdjacencyList[[FaultAgents[[2]]]]];
                   DelaunayEdges = Join[DelaunayEdges,EdgesToAdd];
                   EdgesToAdd = LinkBetwFaultyReliableAgents[FaultAgents[[1]],FaultyAdjacencyList[[FaultAgents[[1]]]],ReliAgents[[1]]];
                   DelaunayEdges = Join[DelaunayEdges,EdgesToAdd];
                   EdgesToAdd = LinkBetwFaultyReliableAgents[FaultAgents[[2]],FaultyAdjacencyList[[FaultAgents[[2]]]],ReliAgents[[1]]];
                   DelaunayEdges = Join[DelaunayEdges,EdgesToAdd];,
                   3,
                   EdgesToAdd=LinkBetwFaultyAgents[FaultAgents[[1]],FaultAgents[[2]],FaultyAdjacencyList[[FaultAgents[[1]]]],FaultyAdjacencyList[[FaultAgents[[2]]]]];
                   DelaunayEdges = Join[DelaunayEdges,EdgesToAdd];
                   EdgesToAdd=LinkBetwFaultyAgents[FaultAgents[[1]],FaultAgents[[3]],FaultyAdjacencyList[[FaultAgents[[1]]]],FaultyAdjacencyList[[FaultAgents[[3]]]]];
                   DelaunayEdges = Join[DelaunayEdges,EdgesToAdd];
                   EdgesToAdd=LinkBetwFaultyAgents[FaultAgents[[2]],FaultAgents[[3]],FaultyAdjacencyList[[FaultAgents[[2]]]],FaultyAdjacencyList[[FaultAgents[[3]]]]];
                   DelaunayEdges = Join[DelaunayEdges,EdgesToAdd];
            ];
            ,
            AddingLink = Table[True,{jj,1,Length[FaultAgents]}];
            While[MemberQ[AddingLink,True] && Criteria !={},
                  Do[
                     If[Not[MemberQ[FaultyAdjacencyList[[FaultAgents[[ss]]]],Criteria[[1,1]]]],
                        AddingLink[[ss]] = False;
                     ];
                  ,{ss,1,Length[FaultAgents]}];
                  Criteria = Complement[Criteria,{Criteria[[1]]}];
            ];
            Do[
               If[AddingLink[[rr]] == True,
                  ToAdd = Complement[subsetsThree[[1]],{FaultAgents[[rr]]}];
                  DelaunayEdges = Join[DelaunayEdges,{{FaultAgents[[rr]],ToAdd[[1]]},{FaultAgents[[rr]],ToAdd[[2]]}}];
               ];
               ,{rr,1,Length[FaultAgents]}];
         ];
      ];
      subsetsThree=Complement[subsetsThree,{subsetsThree[[1]]}];
   ];
   DelaunayEdges = Partition[Union[DelaunayEdges],1];
   Deladjacencylist =  ToAdjacencyLists[Graph[DelaunayEdges,Table[{{0,0}},{rr,1,n}],EdgeDirection->True]];
   Do[
      Deladjacencylist[[ww]] = Sort[Deladjacencylist[[ww]],
         (ArcTan[pointset[[#2]][[1]]-pointset[[ww,1]],pointset[[#2]][[2]]-pointset[[ww,2]]]>ArcTan[pointset[[#1]][[1]]-pointset[[ww,1]],pointset[[#1]][[2]]-pointset[[ww,2]]])&];
   ,{ww,1,n}];
   Return[Deladjacencylist];
];

LinkBetwFaultyAgents[FaultAgent1_,FaultAgent2_,FaultyAdjacencyList1_,FaultyAdjacencyList2_]:= Module[{EdgesToAdd={}},
   If[MemberQ[FaultyAdjacencyList1,FaultAgent2],
      If[FreeQ[FaultyAdjacencyList2,FaultAgent1],
         EdgesToAdd = {{FaultAgent2,FaultAgent1}};
      ];
      ,
      If[MemberQ[FaultyAdjacencyList2,FaultAgent1],
         EdgesToAdd = {{FaultAgent1,FaultAgent2}};,
         EdgesToAdd = {{FaultAgent1,FaultAgent2},{FaultAgent2,FaultAgent1}};
      ];
   ];
   Return[EdgesToAdd];
];

LinkBetwFaultyReliableAgents[FaultAgent_,FaultyAdjacencyList_,ReliAgent_] := Module[{EdgesToAdd={}},
   If[MemberQ[FaultyAdjacencyList,ReliAgent],
      EdgesToAdd = {{ReliAgent,FaultAgent}};,
      EdgesToAdd = {{ReliAgent,FaultAgent},{FaultAgent,ReliAgent}};
   ];
   Return[EdgesToAdd];
];
                                                                      
(* Close up the open environments *)
End[];
EndPackage[];
