function AdjTable = computeAdjTableForFreeCSpacePoints( freeCSpacePoints, maxAdjRadius )

n = size(freeCSpacePoints,1);
AdjTable = {};
AdjTable{n} = [];

d = [];

for i = 1:n
    
    for j = i+1:n
        
        % Compute angular distance ... in principle, this should be a
        % function, but it runs much faster when done here as there are
        % fewer memory allocations.
        d = mod(freeCSpacePoints(i,:) - freeCSpacePoints(j,:), 2*pi);
        % Angular distance is shorter of clockwise and counterclockwise
        % distances.
        d = min( [d; 2*pi - d] );
        d = d.^2;
        d = sqrt(sum(d));
        
        if( d < maxAdjRadius )
        
            % Test intermediate points???
            AdjTable{j} = [AdjTable{j}; i];
            AdjTable{i} = [AdjTable{i}; j];

        end

    end
end

return

