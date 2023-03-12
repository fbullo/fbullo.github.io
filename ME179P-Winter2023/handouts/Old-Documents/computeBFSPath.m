function path = computeBFSPath( start, goal, AdjTable )
% Returns Breadth-First-Search path from start to goal through graph.
% Requires start and goal vertices as well as a adjacency lookup table

BFSEdges = computeBFSEdges(start, AdjTable);

% Initialize path to an empty array so we can fill it out in loop
path = [];

disp('   Found edges, searching for path')
t = cputime;
% Build path backwards from goal to start through the BFS edges
tmp = goal;
bContinue = true;
while bContinue,
  bContinue = false;
  for j=1:size(BFSEdges,1), 
    if (BFSEdges(j,2)==tmp),
       tmp = BFSEdges(j,1);
       path = [path;BFSEdges(j,:)];
       bContinue = true;
       break
    end
  end
end

% Reverse path so that we have a path from start to goal
path = path(end:-1:1,:);
fprintf('Elapsed time is %0.6f secs\n', cputime - t);
