function P = computeBFSpath(AdjTable, start, goal)
% computeBFSpath returns a BFS path from start to goal

% compute BFS tree
parent = computeBFStree(AdjTable, start);

% extract the path from the parent pointers
P = goal; u = goal;
while parent(u) ~= 0;
    u = parent(u);
    P(end + 1) = u;
end
% reverse the path so that it reads from start to goal
P = fliplr(P);

end
