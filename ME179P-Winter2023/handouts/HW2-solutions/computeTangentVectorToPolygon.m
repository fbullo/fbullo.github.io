function outvar = computeTangentVectorToPolygon(q,P)
% computeTangentVectorToPolygon(q,P) returns the unit vector tangent to the
%   polygon defined by P, for a robot at q

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Patrick John Therrien; May 11, 2015
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%initialize outvar and vector variable
outvar = [0,0];
v = outvar; 

% Use computeDistanceToPolygon's built in detection method to determine
% which vertex/segment is closest. 

polyInfo = computeDistancePointToPolygon(q,P);

if polyInfo(2) == 0     %This is the case where closest point is a segment
    p1 = P(polyInfo(3),:);%picks out first point of the segment
    if polyInfo(3) == length(P(:,1))%last segment
        p2 = P(1,:);%first specified point is the second point of the last segment
    else
        p2 = P(polyInfo(3)+1,:);%otherwise, next point is the second segment point
    end
    % u vector is parallel to the segment
    outvar(1) = (p2(1)-p1(1))/pdist([p1;p2]);%normalized
    outvar(2) = (p2(2) - p1(2))/pdist([p1;p2]);
else 
    vPH = P(polyInfo(3),:);%picks the closest vertex
    v(1) = vPH(1) - q(1);
    v(2) = vPH(2) - q(2);
    nV = norm(v);
    % normalize the vector output and rotate so it is tangent to a circle
    % centerd on the vertex
    outvar(1) = v(2)/nV;
    outvar(2) = -v(1)/nV;
end
