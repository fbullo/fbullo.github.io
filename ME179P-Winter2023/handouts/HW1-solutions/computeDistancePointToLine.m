function distance = computeDistancePointToLine(q, p1, p2)
% computeDistancePointToLine returns the distance between point q
%   and line passing through points p1 and p2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cenk Oguz Saglam, January 13, 2013
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Store the x and y components of point p
q_x=q(1);
q_y=q(2);

% Obtain the parameters defining the line passing through points p1 and p2
[a,b,c]=computeLineThroughTwoPoints(p1, p2);

% Calculate the distance
distance=abs(a*q_x+b*q_y+c)/sqrt(a^2+b^2);
