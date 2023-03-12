function minDist = computeDistancePointToPolygon(q, P)
% Computes the distance and closest segment from a point to a polygon.
% Function requires the point to test q = [x,y] and a n x 2 array of
% counterclockwise points, P, defining the polygon.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intro to Robotics, ME170A/ECE181A, Spring 2009
% Joey Durham
% April 8, 2009
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%disp('Using default file')

if( ndims(P) ~= 2 || size(P,1) < 3 || size(P,2) ~= 2)
    error('Polygon format is an n-by-2 array of points where n >= 3');
end

% Initialize variables to store closest segment, minDist is initialized to
% an impossible value so that can be caught in the loop.
minDist = -1;

% Loop over all segments in the polygon, finding the closest one
dist = 0;
for i = 1:length(P) - 1
    dist = getDistancePointToSegment( q, P(i,:), P(i+1,:) );
    
    if( dist < minDist || minDist < 0 )
        minDist = dist;
    end
end

% Function needs to handle the case where polygon has an implied final
% segment closing the loop.
if( any(P(end,:) ~= P(1,:)) )

    dist = getDistancePointToSegment( q, P(end,:), P(1,:) );

    if( dist < minDist || minDist < 0 )
        minDist = dist;
    end
    
end

% Uncomment the following lines to display debug plot
% figure;
% hold on
% fill(P(:,1),P(:,2),'k')
% plot(q(1),q(2),'ob');
% axis square
% hold off;