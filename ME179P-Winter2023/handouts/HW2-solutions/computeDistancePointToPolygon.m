function outvar = computeDistancePointToPolygon(q, P)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Patrick John Therrien, May 11, 2015
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% computeDistancePointToPolygon(q, P) returns the distance between point q
% and the polygon defined by P as its first element. The second element is
% binary and represents whether or not the returned distance is to a vertex
% or a segment. If it is one then the value is the distance to a vertex.
% The third element represents the index within the polygonal list of
% either the vertex or segment which is closest to q. The first segment is
% considered to be the one between the first and second vertices.


% Initialize the distance as infinite and the other values as impossible
% results
outvar = [inf,-1,-2];

% Copy the first point to the end to consider all segments
P(end+1,:)=P(1,:);
% Consider distances to all segments and take the minimum of them
for i=1:length(P(:,1))-1
   p1=P(i,:);
   p2=P(i+1,:);
   dist= computeDistancePointToSegment(q,p1,p2);
   if (dist(1) < outvar(1))
       outvar(1) = dist(1);%New minimum distance established
       outvar(3) = dist(2); % provides info on which segment point was closest
       wPolyP = i;% keeps track of which polygonal segment was closest
   end
end

if outvar(3) > 0 % One of the segment endpoints was closest
    outvar(3) = wPolyP + outvar(3)-1;% Subtract 1 since both wPolyP & outvar(3) are zero indexed
    outvar(2) = 1;%closest to a point
else 
    outvar(3) = wPolyP;%identifies the segment
    outvar(2) = 0;%closest to a segment
end 