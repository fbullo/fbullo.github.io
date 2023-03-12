%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intro to Robotics, ME170A/ECE181A, Spring 2010
% Joey Durham
% May 13, 2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This script tests a polygon collision detector with four scenarios, one
% where some vertices of one polygon are inside the other, one where two
% polygons collide without this being the case, one where two polygons
% do not collide, and one where the two polygons are the same.

close all;

fprintf('\nTesting polygon collision detector\n\n');

Poly1 = [0,0; 4,1; 4,2; 0,1];
Poly2Base = [-2, -1; -1, -1; 0,5; -1, 5];
Poly2a = Poly2Base;

bCollide = checkCollisionBetweenPolygons( Poly1, Poly2a );

if( bCollide )
    fprintf('***Error!  Collision detected when no collision exists***\n');
else
    fprintf('No collision check passed\n');
end

figure;
hold on;
plot( [Poly1(:,1); Poly1(1,1)], [Poly1(:,2); Poly1(1,2)], '-b')
plot( [Poly2a(:,1); Poly2a(1,1)], [Poly2a(:,2); Poly2a(1,2)], '-r')
hold off;

% Slide Poly2 so that some vertices are inside each polygon
Poly2b = Poly2Base + 1.5;

bCollide = checkCollisionBetweenPolygons( Poly1, Poly2b );

if( ~bCollide )
    fprintf('***Error!  Vertex collision not detected***\n');
else
    fprintf('Vertex collision check passed\n');
end

figure;
hold on;
plot( [Poly1(:,1); Poly1(1,1)], [Poly1(:,2); Poly1(1,2)], '-b')
plot( [Poly2b(:,1); Poly2b(1,1)], [Poly2b(:,2); Poly2b(1,2)], '-r')
hold off;

% Slide Poly2 so that there is overlap with Poly1
Poly2c = Poly2Base + 1.5 + [ones(size(Poly2Base,1), 1), -ones(size(Poly2Base,1), 1)];

bCollide = checkCollisionBetweenPolygons( Poly1, Poly2c );

if( ~bCollide )
    fprintf('***Error!  Segment collision not detected***\n');
else
    fprintf('Segment collision check passed\n');
end

figure;
hold on;
plot( [Poly1(:,1); Poly1(1,1)], [Poly1(:,2); Poly1(1,2)], '-b')
plot( [Poly2c(:,1); Poly2c(1,1)], [Poly2c(:,2); Poly2c(1,2)], '-r')
hold off;

bCollide = checkCollisionBetweenPolygons( Poly1, Poly1 );

if( ~bCollide )
    fprintf('***Error!  Coincident collision not detected***\n');
else
    fprintf('Coincident collision check passed\n');
end

%% Time many collision detection runs

fprintf('\nTiming 1000 collision checks\n\n')

t = cputime;
for i = 1:1000
    bCollide = checkCollisionBetweenPolygons( Poly1, Poly2a );
end
timeNoCollision = cputime - t;

fprintf('No collision time: %f secs\n', timeNoCollision);

t = cputime;
for i = 1:1000
    bCollide = checkCollisionBetweenPolygons( Poly1, Poly2b );
end
timeVertexCollision = cputime - t;

fprintf('Vertex collision time: %f secs\n', timeVertexCollision);

t = cputime;
for i = 1:1000
    bCollide = checkCollisionBetweenPolygons( Poly1, Poly2c );
end
timeSegmentCollision = cputime - t;

fprintf('Segment collision time: %f secs\n', timeSegmentCollision);