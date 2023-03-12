%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intro to Robotics, ME170A/ECE181A, Spring 2009
% Joey Durham
% April 15, 2009
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Computes the free configuration space for a simple two link manipulator.

% The two links are modeled as rectangles with semi-circles attached to the
% ends, meaning each link is specified by a length and a radius.
link1Length = 1.0;
link2Length = 1.0;
link1Radius = 0.1;
link2Radius = 0.1;

% The first link is attached to a base at (0,0) while the second is
% attached to the end of the first.
armBase = [0,0];

% To simplify the checks for collisions, the only obstacle in
% the workspace is a circle specified by a center and a radius.
circleCenter = [-1.0,-1.0];
circleRadius = 0.2;

numSamples = 100;
angleRange = linspace( -pi, pi, numSamples );

figure;
axis( [-pi pi -pi pi] )
xlabel('alpha')
ylabel('beta')
hold on

for alpha = angleRange    
    for beta = angleRange
        
        % The two segments of the arm are described by three points, where
        % link1 is between armBase and p1, while link2 is between p1 and p2.
        p1 = armBase + [link1Length*cos(alpha), link1Length*sin(alpha)];
        p2 = p1 + [link2Length*cos(alpha + beta), link2Length*sin(alpha+beta)];
        
        % To check that the two links are not hitting the circle for these
        % values of alpha and beta, it suffices to check that the distance
        % between the center of the circular obstacle and the two segements
        % defining the links are greater than the sum of the radii.
        if( getDistancePointToSegment(circleCenter,armBase,p1) < (link1Radius + circleRadius) )
            plot( alpha, beta, '.b', 'LineWidth', 3 );
        elseif( getDistancePointToSegment(circleCenter,p1,p2) < (link2Radius + circleRadius) )
            plot( alpha, beta, '.r', 'LineWidth', 3 );
        end
        
    end
end

hold off