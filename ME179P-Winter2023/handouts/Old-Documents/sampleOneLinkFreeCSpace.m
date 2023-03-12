%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intro to Robotics, ME170A/ECE181A, Spring 2009
% Joey Durham
% April 13, 2009
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Computes the free configuration space for a one link manipulator.

% The link is modeled as a rectangle with semi-circles attached to the
% ends, meaning it is specified by a length and a radius.
link1Length = 1.0;
link1Radius = 0.1;

% The link is attached to a base at (0,0).
armBase = [0,0];

% To simplify the checks for collisions, the only obstacle in
% the workspace is a circle specified by a center and a radius.
circleCenter = [.5,.5];
circleRadius = 0.2;

numSamples = 100;
angleRange = linspace( -pi, pi, numSamples );

figure;
axis( [-pi pi -pi pi] )
hold on

for alpha = angleRange
        
    % The segment describing the link is defined by two points, armBase
    % and p1.
    p1 = armBase + [link1Length*cos(alpha), link1Length*sin(alpha)];

    % To check that the link is not hitting the circle at this
    % value of alpha, it suffices to check that the distance
    % between the center of the circular obstacle and the segement
    % defining the link is greater than the sum of the radii.
    if( getDistancePointToSegment(circleCenter,armBase,p1) < (link1Radius + circleRadius) )
        plot( alpha, 0, '.b', 'LineWidth', 3 );
    end

end

hold off