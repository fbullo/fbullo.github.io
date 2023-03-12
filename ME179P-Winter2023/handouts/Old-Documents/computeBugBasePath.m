function path = computeBugBasePath( start, goal, PolyList )
% Computes the path a Bug would follow until it hits an obstacle.
% Function requires start and goal point as well as a cell array of
% polygonal obstacles and returns an ordered list of all of the points 
% visited by the bug along its path. Note that each polygon is 
% defined by a n x 2 array of counterclockwise points.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intro to Robotics, ME170A/ECE181A, Spring 2009
% Joey Durham
% April 25, 2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maxStepSize = 0.02;

% Initialize the path variable 
path = start;

% Loop continuously until Bug reaches a termination condition and breaks
% out of the loop
while( true )
    
    distToGoal = dist( path(end,:), goal );
    
    % Has bug reached goal location?
    if( distToGoal < maxStepSize )
        break
    end
    
    distToClosestObstacle = distToGoal + maxStepSize;
    closestPolygon = [];
    
    % Loop through each polygonal obstacle in the list to find the closest
    % one to the Bug's current position.
    for i = 1:length(PolyList)
        distToObstacle = computeDistancePointToPolygon(path(end,:), PolyList{i});
        if( distToObstacle < distToClosestObstacle )
            distToClosestObstacle = distToObstacle;
            closestPolygon = PolyList{i};
        end
    end
    
    % Is bug inside polygon?
    if( length(closestPolygon) > 0 && inpolygon(path(end,1), path(end,2), closestPolygon(:,1), closestPolygon(:,2)) )
        disp('Error - bug entered polygon')
        break
    end
    
    % Limit the length of a step the Bug will take based on proximity to
    % obstacles, goal so that it does not over shoot and enter an obstacle
    % or pass the goal location.
    stepSize = min( [maxStepSize, distToClosestObstacle/5, distToGoal] );
    
    % Has bug hit an obstacle?
    if( distToClosestObstacle < maxStepSize )
        % BugBase simply gives up if it hits an obstacle
        disp('Could not find path to goal!')
        break
    else
        % Step towards goal along unit vector u between current point and goal
        u_x = (goal(1) - path(end,1))/distToGoal;
        u_y = (goal(2) - path(end,2))/distToGoal;

        % Add new step to end of the path
        step = stepSize*[u_x, u_y];
        newPoint = path(end,:) + step;
        path = [path; newPoint];
    end
    
end

return
