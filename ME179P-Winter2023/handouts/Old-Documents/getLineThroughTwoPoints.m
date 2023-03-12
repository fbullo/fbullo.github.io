function [a,b,c] = getLineThroughTwoPoints(p1, p2)
% getLineThroughTwoPoints  returns equation for the line through two points.
%   [a,b,c] = getLineThroughTwoPoints(p1,p2)
%   Function takes two arguments, p1 = [x1,y1] and p2 = [x2,y2],
%   and returns [a,b,c] corresponding to the line equation
%   ax + by + c = 0.  Error when p1 = p2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intro to Robotics, ME170A/ECE181A, Spring 2009
% Joey Durham
% April 1, 2009
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tolerance = 0.0001;

if dist(p1, p2) < 2*tolerance
    error('Cannot compute line, points are too close together')
end

x1 = p1(1);
y1 = p1(2);
x2 = p2(1);
y2 = p2(2);

% Initial values for line variables
a = 1;
b = 1;
c = 0;

denom = x2*y1 - x1*y2;

% Since computations of a and b require divisions, need
% to check that variables aren't too close to zero
if abs(denom) > tolerance
    a = (y2 - y1)/denom;
    b = (x1 - x2)/denom;
    c = 1;
elseif abs(x1) > tolerance
    a = -y1/x1;
elseif abs(x2) > tolerance
    a = -y2/x2;
elseif abs(y1) > tolerance
    b = -x1/y1;
elseif abs(y2) > tolerance
    b = -x2/y2;
else
    error('Cannot compute line, points are too close to 0,0')
end

% Line definition does not change with scaling
scale = sqrt(a^2 + b^2);
a = a / scale;
b = b / scale;
c = c / scale;

%% Uncomment the following lines to display debug plot of line
% figure;
% hold on
% axis([min(x1,x2)-3, max(x1,x2)+3, min(y1,y2)-3, max(y1,y2)+3]);
% plot(x1,y1,'ob');
% plot(x2,y2,'ob');
% slope = -a/b;
% intercept = -c/b;
% refline(slope, intercept);
% hold off;