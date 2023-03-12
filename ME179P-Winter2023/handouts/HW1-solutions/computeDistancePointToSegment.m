function outvar = computeDistancePointToSegment(q, p1, p2)
% computeDistancePointToSegment returns the distance between point q
%   and the segment between points p1 and p2. It also returns a variable wP
%   which tells if the closest point is on the segment (wP = 0), is closest
%   to the first point (wP = 1) or the second (wP = 2). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Patrick John Therrien, May 11, 2015
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Writing the line as p1 + u(p2-p1) we can find u as:
segmentLength = pdist([p1;p2]);

u = ((q(1) - p1(1))*(p2(1) - p1(1)) + (q(2) - p1(2))*(p2(2)-p1(2)))/(segmentLength^2);

% If the projection line is between the endpoints then return the distance
% to line otherwise return distance to closest endpoint.

if 0 <= u && u <= 1
    D = computeDistancePointToLine(q,p1,p2);
    wP = 0;
elseif u < 0
    D = pdist([q;p1]);
    wP = 1;
else 
    D = pdist([q;p2]);
    wP = 2;
end
outvar = [D,wP];

end