function path = computeBug1Path( start, goal, PolyList )
% Computes the path from start to goal using the Bug1 algorithm.
% Function requires start and goal point as well as a cell array of
% polygonal obstacles and returns an ordered list of all of the points 
% visited by the bug along its path.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intro to Robotics, ME170A/ECE181A, Spring 2009
% Joey Durham
% April 25, 2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maxStepSize = 0.02;
count = 0;
path = start;
behaviorState = 1;
hitPoint = [];
leavePoint = [];

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
    
    stepSize = min( [maxStepSize/2, distToClosestObstacle/5, distToGoal] );
    
    % Has bug hit an obstacle?
    if( behaviorState == 1 && distToClosestObstacle < maxStepSize )
        disp('Switching to SEARCH FOR leave point')
        behaviorState = 2;
        hitPoint = path(end,:);
    elseif( behaviorState == 2 && dist(path(end,:), hitPoint) > 2.0*maxStepSize )
        % Bug has left initial hit point
        behaviorState = 3;
    elseif( behaviorState == 3 && length(leavePoint) > 0 && dist(path(end,:), hitPoint) < 1.5*maxStepSize )
        % Bug has come back to initial hit point
        if( dist(hitPoint, leavePoint) < maxStepSize )
            disp('Could not find path to goal!')
            break
        else
            disp('Switching to MOVE TO leave point')
            behaviorState = 4;
        end
        
    elseif( behaviorState == 4 && dist(path(end,:),leavePoint) < 1.5*maxStepSize )
        disp('Switching out of obstacle behavior')

        behaviorState = 1;
        
        hitPoint = [];
        leavePoint = [];
    end
        
    if( behaviorState > 1 )
        % Take step along tangent line to polygon
        [u_x, u_y] = computeTangentVectorToPolygon( path(end,:), closestPolygon );
        
        step = stepSize*[u_x, u_y];
        
        if( behaviorState < 4 )
            if( (length(leavePoint) == 0 || dist(path(end,:),goal) < dist(leavePoint,goal)) )
                leavePoint = path(end,:);
            end
        else
            if( distToClosestObstacle > 1.5*maxStepSize )
                % Incase robot is drifting away from obstacle while 
                % looping, force a correction so it doesn't miss leave point
                deltaX = leavePoint(1) - path(end,1);
                deltaY = leavePoint(2) - path(end,2);

                %step = stepSize*[deltaX, deltaY]/distToGoal;
            end
        end
        
        path = [path; path(end,:) + step];
        
    else
        % Step towards goal along line between current point and goal
        deltaX = goal(1) - path(end,1);
        deltaY = goal(2) - path(end,2);

        step = stepSize*[deltaX, deltaY]/distToGoal;
        path = [path; path(end,:) + step];
    end
    
    count = count + 1;
    
end

return
