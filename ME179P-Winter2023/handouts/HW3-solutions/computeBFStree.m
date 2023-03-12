function parent = computeBFStree(AdjTable, start)
% computeBFStree returns a vector of pointers describing the BFS tree rooted at start

% initialize queue
Q = [start];
% initialize parent pointers.  We use infinity for NIL and 0 for SELF
n= length(AdjTable);
parent = inf(n, 1);  parent(start) = 0;

while ~isempty(Q);    
% pick first vertex and remove it from list
    v = Q(1);  Q(1) = [];
    
    for u = AdjTable{v}; % loop over all neighbors of v
        if parent(u) == inf; % check if we've found a new vertex
            parent(u) = v; 
            Q(end + 1) = u; 
        end  % if
    end  % for
end  % while
