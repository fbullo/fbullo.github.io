%%%% Matlab Code
%%%% to manipulate a list (and implement a queue)

% column vector
vlist = [20; 21]
size(vlist,1)

% add an element at the end
vlist = [vlist; 22]
size(vlist,1)

% removes first element
vlist(1,:) = []
size(vlist,1)
