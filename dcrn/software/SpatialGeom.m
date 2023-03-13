(*
 *     SpatialGeom.m - a Mathematica Package 
 *     "Some 3D Spatial Geometry Primitives"
 *
 *     Jorge Cortes
 *	
 *     Mechanical and Environmental Engineering
 *     University of California at Santa Barbara
 *
 *     Coordinated Science Laboratory
 *     University of Illinois at Urbana Champaign
 * 			   
 * Copyright (c) 2004, Jorge Cortes
 * Permission is granted to copy, modify and redistribute this file, 
 * provided that this header message is retained.
 *
 * Revision history:
 *
 * Version 0.02 (July 04)
 * Version 0.01 (July 04)
 *
 *)

BeginPackage["SpatialGeom`"];

Needs["DiscreteMath`Combinatorica`"];

(* Eps *)
ToleranceSG=10^(-9);

BisectorTwoPts3D                ::usage = "BisectorTwoPts3D[p1, p2] -> {plane}. Warning: p1 = p2. p1 lies in positive halfplane.";
PlaneThreePts3D                 ::usage = "PlaneThreePts3D[p1,p2,p3] -> {plane}. Warning: p1,p2,p3 aligned.";
LineToPlanes                    ::usage = "LineToPlanes[line3D] -> {plane1,plane2}. line3D is of the form {vector,point}.";
IntersectSegSphereBoundary      ::usage = "IntersectSegSphereBoundary[segment,sphere] ->{points}.";
IntersectSegSphere              ::usage = "IntersectSegSphere[segment,sphere] ->{points}.";
SmallestSphereThroughTwoPts     ::usage = "SmallestSphereThroughTwoPts[p1,p2] -> {sphere}.";
SmallestSphereThroughThreePts   ::usage = "SmallestSphereThroughThreePts[p1,p2,p3] -> {sphere}.";
SphereThroughFourPts            ::usage = "SphereThroughFourPts[p1,p2,p3,p4] -> {sphere}.";
PointInSphere                   ::usage = "PointInSphere[p,sphere] -> {True,False}.";
CircumSphere3D                  ::usage = "CircumSphere3D[pointset] -> {circumsphere}.";

ToAdjacencyLists3D              ::usage = "ToAdjacencyLists3D[graph] -> adjacencylist. graph is of the form {edges,vertices}.";
GraphIntersection3D             ::usage = "GraphIntersection3D[graph3D1,graph3D2] -> graph3D. Assumes graphs have same vertices";
PlotGraph3D                     ::usage = "PlotGraph3D[graph3D] -> graphics3D"
ProxGraphDisk3D                 ::usage = "ProxGraphDisk3D[pointset,radius] -> {edges,vertices}.";
ProxGraphGabriel3D              ::usage = "ProxGraphGabriel3D[pointset] -> {edges,vertices}.";
ProxGraphRelNhb3D               ::usage = "ProxGraphRelNhb3D[pointset] -> {edges,vertices}.";

Begin["`Private`"];

BisectorTwoPts3D[p1_,p2_] :=  Module[{dist=(p2[[1]]-p1[[1]])^2+(p2[[2]]-p1[[2]])^2+(p2[[3]]-p1[[3]])^2},
 If[dist<ToleranceSG, 
   Print["Warning in BisectorTwoPts3D: No unique bisector of same point."]; Return[{}]; , 
   Append[p1-p2,(p2-p1).(p1+p2)/2]/Sqrt[dist] ]
];

PlaneThreePts3D[p1_,p2_,p3_] :=  Module[{normal = Cross[p2-p1,p3-p1]},
 If[Norm[normal]<ToleranceSG, 
   Print["Warning in PlaneThreePts3D: No unique plane through three aligned points."]; Return[{}]; , 
   Append[normal,- normal.p1]/Norm[normal]]
];

LineToPlanes[line3D_]:= Module[{vx=line3D[[1,1]],vy=line3D[[1,2]],vz=line3D[[1,3]],
                                            x0=line3D[[2,1]],y0=line3D[[2,2]],z0=line3D[[2,3]],plane1,plane2,plane3},
   If[vx^2 + vy^2 + vz^2 <= ToleranceSG,
      Print["Warning in LineToPlanes: No unique line through same point."]; Return[{}]; ,
      plane1 = {vy,-vx,0, -vy x0 + vx y0}; plane2 = {vz,0,-vx, -vz x0 + vx z0}; plane3 = {0,vz,-vy, -vz y0 + vy z0};
      If[plane1=={0,0,0,0},Return[{plane2,plane3}],
         If[plane2=={0,0,0,0},Return[{plane1,plane3}],Return[{plane1,plane2}]];
      ];
   ];
];

IntersectSegSphereBoundary[segment_,sphere_]:= Module[{p1=segment[[1]],p2=segment[[2]],line3D,plane1,plane2,a1,b1,c1,d1,a2,b2,c2,d2,
                                    center=sphere[[1]],radius=sphere[[2]],intersection,qinseg={},qonline1,qonline2,dist1,dist2},
   line3D = {p2-p1,p1};
   {plane1,plane2} = LineToPlanes[line3D];
   a1 = plane1[[1]]; b1 = plane1[[2]]; c1 = plane1[[3]]; d1 = plane1[[4]];
   a2 = plane2[[1]]; b2 = plane2[[2]]; c2 = plane2[[3]]; d2 = plane2[[4]];
   intersection = NSolve[{a1 x + b1 y + c1 z + d1==0, a2 x + b2 y + c2 z + d2==0,
                                         (x-center[[1]])^2+(y-center[[2]])^2+(z-center[[3]])^2 ==radius^2},{x,y,z}];
   qonline1 = intersection[[1,{1,2,3},2]]; qonline2 = intersection[[2,{1,2,3},2]];
   If[Norm[Im[qonline1]] >= ToleranceSG,Return[{}],
      dist1 = Max[Norm[qonline1-p1],Norm[qonline1-p2]];
      dist2 = Max[Norm[qonline2-p1],Norm[qonline2-p2]];
      If[dist1 <= Norm[p1-p2],
         qinseg = Append[qinseg,qonline1];
      ];
      If[(qonline1 != qonline2)&&(dist2 <= Norm[p1-p2]),
          If[Norm[qonline1-p1]<Norm[qonline2-p1],qinseg = Append[qinseg,qonline2];,qinseg = Prepend[qinseg,qonline2];];
      ];
      Return[qinseg];
   ];
];

IntersectSegSphere[segment_,sphere_]:= Module[{p1=segment[[1]],p2=segment[[2]],line3D,plane1,plane2,a1,b1,c1,d1,a2,b2,c2,d2,
                          pointsinline,center=sphere[[1]],radius=sphere[[2]],intersection={},qinseg={},qonline1,qonline2,dist1,dist2},
   If[Norm[p1-center]<=radius && Norm[p2-center]<=radius,
      intersection = segment;,
      If[Norm[p2-center]<=radius,{p1,p2}={p2,p1};];
      line3D = {p2-p1,p1};
      {plane1,plane2} = LineToPlanes[line3D];
      a1 = plane1[[1]]; b1 = plane1[[2]]; c1 = plane1[[3]]; d1 = plane1[[4]];
      a2 = plane2[[1]]; b2 = plane2[[2]]; c2 = plane2[[3]]; d2 = plane2[[4]];
      pointsinline = NSolve[{a1 x + b1 y + c1 z + d1==0, a2 x + b2 y + c2 z + d2==0,
                                         (x-center[[1]])^2+(y-center[[2]])^2+(z-center[[3]])^2 ==radius^2},{x,y,z}];
      qonline1 = pointsinline[[1,{1,2,3},2]]; qonline2 = pointsinline[[2,{1,2,3},2]];
      If[Norm[Im[qonline1]] >= ToleranceSG,Return[{}],
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

SmallestSphereThroughTwoPts[p1_,p2_]:={(p1+p2)/2,Norm[p1-p2]/2};

SmallestSphereThroughThreePts[p1_,p2_,p3_] := Module[{a1,b1,c1,d1,a2,b2,c2,d2,a3,b3,c3,d3},
   {a1,b1,c1,d1} = PlaneThreePts3D[p1,p2,p3];
   {a2,b2,c2,d2} = BisectorTwoPts3D[p1,p2];
   {a3,b3,c3,d3} = BisectorTwoPts3D[p1,p3];
   center= Flatten[NSolve[{a1 x + b1 y + c1 z + d1 ==0, a2 x + b2 y + c2 z + d2 ==0, a3 x + b3 y + c3 z + d3 ==0},{x,y,z}]];
   center = {center[[1,2]],center[[2,2]],center[[3,2]]};
   radius = Norm[center-p1];
   Return[{center,radius}];
];

SphereThroughFourPts[p1_,p2_,p3_,p4_]:= Module[{a1,b1,c1,d1,a2,b2,c2,d2,a3,b3,c3,d3},
   {a1,b1,c1,d1} = BisectorTwoPts3D[p1,p2];
   {a2,b2,c2,d2} = BisectorTwoPts3D[p2,p3];
   {a3,b3,c3,d3} = BisectorTwoPts3D[p3,p4];
   center= Flatten[NSolve[{a1 x + b1 y + c1 z + d1 ==0, a2 x + b2 y + c2 z + d2 ==0, a3 x + b3 y + c3 z + d3 ==0},{x,y,z}]];
   center = {center[[1,2]],center[[2,2]],center[[3,2]]};
   radius = Norm[center-p1];
   Return[{center,radius}];
];

PointInSphere[p_,sphere_]:= TrueQ[Norm[p-sphere[[1]]] <=sphere[[2]]];

CircumSphere3DThreePts[p1_,p2_,p3_]:=Module[{sphere12=SmallestSphereThroughTwoPts[p1,p2],
                                              sphere13=SmallestSphereThroughTwoPts[p1,p3],sphere23=SmallestSphereThroughTwoPts[p2,p3]},
   Criteria = Flatten[Position[{PointInSphere[p3,sphere12],PointInSphere[p2,sphere13],PointInSphere[p1,sphere23]},True]];
   If[Criteria !={},
      Switch[Criteria[[1]],1,Return[sphere12],2,Return[sphere13],3,Return[sphere23]];,
      Return[SmallestSphereThroughThreePts[p1,p2,p3]];      
   ];
]

CircumSphere3DFourPts[p1_,p2_,p3_,p4_]:=Module[{sphere123=CircumSphere3DThreePts[p1,p2,p3],
                        sphere124=CircumSphere3DThreePts[p1,p2,p4],sphere134 = CircumSphere3DThreePts[p1,p3,p4],
                        sphere234 = CircumSphere3DThreePts[p2,p3,p4]},
   Criteria = Flatten[Position[{PointInSphere[p4,sphere123],PointInSphere[p3,sphere124],PointInSphere[p2,sphere134],PointInSphere[p1,sphere234]},True]];
   If[Criteria !={},
      Switch[Criteria[[1]],1,Return[sphere123],2,Return[sphere124],3,Return[sphere134],4,Return[sphere234]];,
      Return[SphereThroughFourPts[p1,p2,p3,p4]];      
   ];
]

CircumSphere3D[pointset_]:=Module[{n,pointsetDist=Union[pointset],pointsetCH,pointsetReduced,subsetsFour,
                                          kk,ss,FourPts,SphereToCheck,PtsToCheck,Criteria},
   If[Length[pointsetDist]==1,Return[{pointsetDist[[1]],0}]];
   If[Length[pointsetDist]==2,Return[SmallestSphereThroughTwoPts[pointsetDist[[1]],pointsetDist[[2]]]]];
   If[Length[pointsetDist]==3,Return[CircumSphere3DThreePts[pointsetDist[[1]],pointsetDist[[2]],pointsetDist[[3]]]]];
   If[Length[pointsetDist]==4,Return[CircumSphere3DFourPts[pointsetDist[[1]],pointsetDist[[2]],pointsetDist[[3]],pointsetDist[[4]]]]];
   (* This breaks because of ConvexHull3D
   pointsetCH = ConvexHull3D[pointset];
   pointsetReduced = Union[Flatten[pointsetCH[[Table[kk,{kk,1,Length[pointsetCH]}],1]],1]];
   *)
   pointsetReduced = Union[pointset];                                
   n = Length[pointsetReduced];
   subsetsFour = KSubsets[Table[kk,{kk,1,n}],4];
   ss=1;
   While[ss <= Length[subsetsFour],
      FourPts = pointsetReduced[[subsetsFour[[ss]]]];
      SphereToCheck = CircumSphere3DFourPts[FourPts[[1]],FourPts[[2]],FourPts[[3]],FourPts[[4]]];
      PtsToCheck = pointsetReduced[[Complement[Table[kk,{kk,1,n}],subsetsFour[[ss]]]]];
      Criteria = MemberQ[MapThread[PointInSphere,{PtsToCheck,Table[SphereToCheck,{rr,1,n-4}]}],False];
      If[Criteria,ss = ss +1,Return[SphereToCheck]];
   ];
]

ToAdjacencyLists3D[graph3D_]:= Module[{NumberVertices=Length[graph3D[[2]]],ii,edges,vertices,AdjacencyLists3D},
   edges=Partition[graph3D[[1]],1];
   vertices = Partition[Table[{0,0},{ii,1,NumberVertices}],1];
   AdjacencyLists3D=ToAdjacencyLists[Graph[edges,vertices]];
   Return[AdjacencyLists3D];
];

GraphIntersection3D[graph3D1_,graph3D2_]:= Module[{edges1=graph3D1[[1]],edges2=graph3D2[[1]],vertices=graph3D1[[2]],edges},
   edges = Intersection[edges1,edges2];
   Return[{edges,vertices}];
];

PlotGraph3D[graph3D_,options__]:=Module[{adjacencylists,vertices = graph3D[[2]],n=Length[graph3D[[2]]],kk,image},
   adjacencylists= ToAdjacencyLists3D[graph3D];
   tmp = Table[Cuboid[vertices[[kk]]-.25,vertices[[kk]]+.25],{kk,1,n}];
   Do[
      tmp = Append[tmp,{Thickness[.0035],GrayLevel[.6],Table[Line[{vertices[[ii]],vertices[[adjacencylists[[ii,jj]]]]}],{jj,1,Length[adjacencylists[[ii]]]}]}];
   ,{ii,1,n}];
   image=Show[Graphics3D[tmp],AxesEdge -> {{-1, -1}, {1,-1}, {-1,-1}},Axes->True,AxesLabel->{"x","y","z"},Ticks->None,ViewCenter->{.45,.45,.45},options];
   Return[image];
   ];

ProxGraphDisk3D[pointset_,radius_]:= Module[{edges,edgesCheck,n=Length[pointset],ii,tt,kk},
   edgesCheck = Flatten[NonPositive[Table[Table[If[Norm[ pointset[[tt]]- pointset[[ii]]] < ToleranceSG,1,Norm[ pointset[[tt]]- pointset[[ii]]] - radius - ToleranceSG],{ii,tt+1,n}],{tt,1,n-1}]]];
   edges = Delete[KSubsets[Table[kk,{kk,1,n}],2],Position[Flatten[edgesCheck],False]];
   Return[{edges,pointset}];
];

ProxGraphGabriel3D[pointset_]:= Module[{edges,edgesPruned,n=Length[pointset],tt,kk,PointsToCheck,spherec,spherer},
   edges = edgesPruned= KSubsets[Table[kk,{kk,1,n}],2];
   Do[
      PointsToCheck = pointset[[edges[[tt]]]];
      {spherec,spherer} = SmallestSphereThroughTwoPts[PointsToCheck[[1]],PointsToCheck[[2]]];
      If[MemberQ[NonNegative[Table[Norm[spherec - pointset[[rr]]] - spherer + ToleranceSG,{rr,1,n}]],False],
         edgesPruned = DeleteCases[edgesPruned,edges[[tt]]];
      ];
   ,{tt,1,Length[edges]}];
   Return[{edgesPruned,pointset}];
];

ProxGraphRelNhb3D[pointset_]:= Module[{edges,edgesPruned,n=Length[pointset],tt,kk,rr,PointsToCheck,spherec1,spherec2,dist},
   edges = edgesPruned= KSubsets[Table[kk,{kk,1,n}],2];
   Do[
      PointsToCheck = pointset[[edges[[tt]]]];
      spherec1 = PointsToCheck[[1]];
      spherec2 = PointsToCheck[[2]];
      dist = Norm[spherec1 - spherec2];
      Do[ 
         If[(Norm[spherec1-pointset[[rr]]] - dist + ToleranceSG<0) && (Norm[spherec2-pointset[[rr]]] - dist + ToleranceSG<0),
            edgesPruned = DeleteCases[edgesPruned,edges[[tt]]];            
         ];
      ,{rr,1,n}];
   ,{tt,1,Length[edges]}];
   Return[{edgesPruned,pointset}];
];

(*

ConvexHull3D breaks with this set (even if dropping the redundant points)

pointset={{0.882636, 0.144282, -0.397216}, {0.823114, -0.0170443, -0.404637},
    {0.850224, 0.0566953, -0.40135}, {0.792121, -0.102262, -0.408046}, {0.882636, 0.144282, -0.397216},
    {0.953691, 0.338098, -0.388821}, {0.863533, 0.0930007, -0.399789}, {0.858352, 0.0782959, -0.400183},
    {0.823114, -0.0170443, -0.404637}, {0.993073, 0.445682, -0.38421}, {0.768822, -0.166214, -0.410646},
    {0.882636, 0.144282, -0.397216}, {0.882636, 0.144282, -0.397216}, {0.829226, -0.00145214, -0.403509},
    {0.845522, 0.0442518, -0.40206}, {0.889341, 0.163649, -0.396903}, {0.882636, 0.144282, -0.397216},
    {0.882636, 0.144282, -0.397216}, {0.940105, 0.302834, -0.391236}, {0.882636, 0.144282, -0.397216},
    {0.882636, 0.144282, -0.397216}, {0.858352, 0.0782959, -0.400183}, {0.934058, 0.284897, -0.391262}}

*)

(* Close up the open environments *)
End[];
EndPackage[];

