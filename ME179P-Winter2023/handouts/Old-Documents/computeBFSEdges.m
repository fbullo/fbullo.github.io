function BFSEdges = computeBFSEdges( start, AdjTable )
% Computes the tree of Breath-First-Search edges from a start node.
% Requires a start node index, and an adjacency lookup table

% number of nodes
n = length(AdjTable);

%% mark all nodes as unvisited, except for start
marked = -1*ones(n,1);  marked (start) = 0;

%% prepare list of vertices to visit and BFS edges
VertexList = [start];  BFSEdges   = [];

%% loop while the list of vertices to visit is nonempty
while length(VertexList)>0, 

 %% pick first vertex and remove it from list
 v = VertexList(1,:);  VertexList(1,:) = [];

 %% find all neighbors of vertex that are unmarked
 for i=1:length(AdjTable{v})
 
  idx = AdjTable{v}(i);

  if (marked(idx) == -1),
   % found an unmarked neighbor, hence mark it
   % and add it to list of vertices to explore from
   % and save the useful edge
   marked(idx) = marked(v)+1;
   VertexList = [VertexList; idx];
   BFSEdges = [BFSEdges; v, idx];

  end %if
 end %for
end %while

