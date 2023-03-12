function [u_x, u_y] = computeTangentVectorToPolygon(q, P)
% Computes the right-turning tangent vector to a polygon from a point.
% Function requires the point to test q = [x,y] and a n x 2 array of
% counterclockwise points, P, defining the polygon.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intro to Robotics, ME170A/ECE181A, Spring 2009
% Joey Durham
% Created: April 8, 2009
% Modified: April 21, 2014 by Rush Patel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%disp('Using default file')

tol = 0.001; %Tolerance to account for numerical errors

% Initialize variables to store closest segment, minDist is initialized to
% an impossible value so that can be caught in the loop.
minDist = -1;
minSeg = [0, 0;0, 0];

% Loop over all segments in the polygon, finding the closest one
dist = 0;
for i = 1:length(P) - 1
    dist = getDistancePointToSegment( q, P(i,:), P(i+1,:) );
    
    if( dist < minDist || minDist < 0 )
        minDist = dist;
        minSeg(1,:) = P(i,:);
        minSeg(2,:) = P(i+1,:);
    end
end

% Function needs to handle the case where polygon has an implied final
% segment closing the loop.
if( any(P(end,:) ~= P(1,:)) )

    dist = getDistancePointToSegment( q, P(end,:), P(1,:) );

    if( dist < minDist || minDist < 0 )
        minDist = dist;
        minSeg(1,:) = P(end,:);
        minSeg(2,:) = P(1,:);
    end
    
end

p1 = minSeg(1,:);
p2 = minSeg(2,:);

% Need line defining closest segment to determine tangent
[a, b, c] = getLineThroughTwoPoints(p1, p2);

% There are two cases to consider: If orthogonal projection of q is on 
% segment, use slope of segment.  Otherwise, use tangent to circle around
% closest end point.
p_x = (b^2*q(1) - a*b*q(2) -a*c)/sqrt(a^2 + b^2);
p_y = (-a*b*q(1) + a^2*q(2) -b*c)/sqrt(a^2 + b^2);

bOnX = p_x >= (min(p1(1),p2(1))-tol) && p_x <= (max(p1(1),p2(1))+tol);
bOnY = p_y >= (min(p1(2),p2(2))-tol) && p_y <= (max(p1(2),p2(2))+tol);

u_x = 1;
u_y = 1;
goTowardPoint = [0 0];

if( (bOnX && bOnY) )
    % Use vector from projected point to right endpoint of segment, meaning
    % this is a right-turning function. Use p1-p2 instead to turn left.
    % Also note using p2-p1 is actually left turning if q is inside of P.   
	u_x = p2(1) - p1(1);
	u_y = p2(2) - p1(2);
else
	% q is closest to a vertex, need to find which vertex
	goTowardPoint = p1;
	if( sqrt((p1(1) - q(1))^2 + (p1(2) - q(2))^2) > minDist + tol )
		goTowardPoint = p2;
	end
	
	% For right turning robot, find tangent vector to circle by
    % translating so q is at the origin, and then rotating p by -90 
    % about q.  Switch signs (ie, rotate p by 90) for left turning.
	u_x = (goTowardPoint(2) - q(2));
	u_y = -(goTowardPoint(1) - q(1));
    
    if( u_x^2 + u_y^2 < tol^2 )
        % q is on vertex, need to head towards next vertex in list.
        % Head towards previous for left-turning.
        for j = 1:length(P)
            if( P(j,:) == goTowardPoint )
                goTowardPoint = P(1+mod(j,length(P)),:);
                u_x = goTowardPoint(1) - p_x;
                u_y = goTowardPoint(2) - p_y;
                break;
            end
        end
    end
    
end

% Need to scale to give unit vector
div = sqrt(u_x^2 + u_y^2);
u_x = u_x / div;
u_y = u_y / div;

% Uncomment the following lines to display debug plot
% figure;
% hold on
% fill(P(:,1),P(:,2),'k')
% plot(q(1),q(2),'ob');
% plot([q(1), q(1) + u_x],[q(2), q(2) + u_y],'-b');
% axis square
% hold off;