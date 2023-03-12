function d = angDist( test1, test2 )

% For each dimension, angular distance is shorter of clockwise and
% counterclockwise distance
d = mod(test1 - test2, 2*pi);
%dCC = 2*pi - dC;
d = min( [d; 2*pi - d] );
d = d.^2;
d = sqrt(sum(d));

return