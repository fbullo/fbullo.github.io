function freeConfigs = computeTwoPolyLinkFreePoints( sampleList, link1BaseRef, link1PolyRef, link2BaseRef, link2PolyRef, ObstacleList )
% Determines which samples points are free for a two link manipulator.
% Function requires a list of (alpha, beta) points to test, plus definitions of the
% polygons representing the links and a list of polygonal obstacles.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intro to Robotics, ME170A/ECE181A, Spring 2009
% Joey Durham
% May 5, 2009
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The two links are modeled as polygons defined by the arguments to the function.
% The first link is attached to a base at link1BaseRef while the second is
% attached to a point on the first defined by link2BaseRef

freeConfigs = [];

for i = 1:size(sampleList,1)
    
    alpha = sampleList(i,1);
    beta = sampleList(i,2);
    
    [link1Poly, link2Base] = displaceLinkPoly( link1PolyRef, alpha, link1BaseRef, link2BaseRef );
        
    [link2Poly, link3Base] = displaceLinkPoly( link2PolyRef, alpha + beta, link2Base, [0 0] );
    
    bFree = true;
    for k = 1:length(ObstacleList)
        if( checkCollisionBetweenPolygons( link1Poly, ObstacleList{k} ) )
            bFree = false;
            break;
        elseif( checkCollisionBetweenPolygons( link2Poly, ObstacleList{k} ) )
            bFree = false;
            break;
        end
    end
    
    if( bFree )
        freeConfigs = [freeConfigs; alpha, beta];
    end

end