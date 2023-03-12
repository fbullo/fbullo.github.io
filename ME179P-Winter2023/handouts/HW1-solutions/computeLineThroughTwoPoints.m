function [a,b,c] = computeLineThroughTwoPoints(p1, p2)
% computeLineThroughTwoPoints  returns equation for the line through two points.
%   [a,b,c] = computeLineThroughTwoPoints(p1,p2)
%   Function takes two arguments, p1 = [x1,y1] and p2 = [x2,y2],
%   and returns [a,b,c] corresponding to the line equation
%   ax + by + c = 0.  Error when p1 = p2.

tolerance = 0.1^7;
distance = sqrt((p1(1)-p2(1))^2 + (p1(2)-p2(2))^2);
if distance < tolerance
    error('Cannot compute line, points are too close together')
end

a = p1(2)-p2(2);
b = p2(1)-p1(1);
c = p1(1)*p2(2) - p2(1)*p1(2);

% Line definition does not change with re-scaling
a = a / distance; b = b / distance; c = c / distance;
