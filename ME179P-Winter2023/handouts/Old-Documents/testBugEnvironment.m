%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intro to Robotics, ME170A/ECE181A, Spring 2009
% Joey Durham
% April 25, 2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define start and goal positions plus a couple of polygonal obstacles
start = [.3,.3]
goal = [5.4,4.2]
Poly1 = [-0.6,0.3;-0.4,-0.4;0.7,-0.3;0.6,0.4;0.2,0.3;-0.296057,0.596997];
Poly2 = [-0.8,-0.4;-0.1,-0.1;0.9,-0.4;0.3,0.2;0.102922,0.598169;-0.3,0.4];

% Shift polygons into positions along path to create obstacles
Poly1 = 1.5*Poly1 + 1.5;
Poly2 = 2.0*Poly2 + 3.5;

% Need to create a list of these obstacles to define the environment, the curly
% brackets define a MATLAB cell array which allows us to easily handle
% polygons of different lengths
PolyList = {Poly1, Poly2};

% Bug algorithm invocation goes here, tic and toc measure elapsed time
tic
path = computeBug1Path( start, goal, PolyList );
toc

stepSizes = path(2:end,:)-path(1:end-1,:);
totalPathLength = 0;
for i=1:length(stepSizes)
  totalPathLength = totalPathLength + norm(stepSizes(i,:));
end
totalPathLength

% Plot path of Bug
figure;
hold on
for i = 1:length(PolyList)
    fill(PolyList{i}(:,1), PolyList{i}(:,2), 'k');
end
plot( path(:,1), path(:,2), '-b' );
plot( start(1), start(2), 'og' );
plot( goal(1), goal(2), 'or' );
title( 'Bug path' );
hold off

% Plot distance to goal
distanceToGoal = zeros(length(path),1);
for i = 1:length(path)
    distanceToGoal(i) = dist(path(i,:), goal);
end
figure;
hold on
plot( distanceToGoal, '-k' );
title( 'Bug Distance to Goal' );
xlabel('Algorithm steps');
ylabel('Distance to Goal (m)');
hold off
