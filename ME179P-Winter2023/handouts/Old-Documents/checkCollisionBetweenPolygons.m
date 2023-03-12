function bCollide = checkCollisionBetweenPolygons( P1,P2 )
% Returns a boolean indicating whether two arbitrary polygons collide.
% Requires to polygons defined as n-by-2 arrays of counterclockwise points.
% Function first tests whether any vertices of one polygon are inside the
% other, then checks for intersection of segments.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intro to Robotics, ME170A/ECE181A, Spring 2009
% Joey Durham
% April 21, 2009
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tolerance = 0.001;
bCollide = false;

% First, check if any vertex of P1 is inside P2
in2 = inpolygon( P1(:,1), P1(:,2), P2(:,1), P2(:,2) );

if( any(in2) )
    %fprintf('Vertices in polygon\n');
    bCollide = true;
    return
end

% Next, check if any vertex of P2 is inside P1
in1 = inpolygon( P2(:,1), P2(:,2), P1(:,1), P1(:,2) );

if( any(in1) )
    %fprintf('Vertices in polygon\n');
    bCollide = true;
    return
end

% Finally, loop over all combinations of edges of segments checking for
% intersection.  s1 and s2 are the endpoints of the ith segment of P1, while t1
% and t2 are for the jth segment of P2
for i = 1:size(P1,1)
    s1 = P1(1 + mod(i-1,size(P1,1)), :);
    s2 = P1(1 + mod(i,size(P1,1)), :);
    
    for j = 1:size(P2,1)
        t1 = P2(1 + mod(j-1,size(P2,1)), :);
        t2 = P2(1 + mod(j,size(P2,1)), :);
        
        den = (t2(2)-t1(2))*(s2(1)-s1(1)) - (t2(1)-t1(1))*(s2(2)-s1(2));
        
        u_a = (t2(1)-t1(1))*(s1(2)-t1(2)) - (t2(2)-t1(2))*(s1(1)-t1(1));
        
        if( abs(den) < tolerance )
            if( abs(u_a) < tolerance )
                % Segments are coincident
                %fprintf('Coincident segments\n');
                bCollide = true;
                return
            else
                % Segments are parallel and cannot intersect
                continue
            end
        end
        
        u_b = (s2(1)-s1(1))*(s1(2)-t1(2)) - (s2(2)-s1(2))*(s1(1)-t1(1));
        
        u_a = u_a / den;
        u_b = u_b / den;
        
        if( u_a >= 0 && u_a <= 1 && u_b >= 0 && u_b <= 1 )
            % Segments intersect
            %fprintf('Intersecting segments\n');
            bCollide = true;
            return
        end
    end
end
