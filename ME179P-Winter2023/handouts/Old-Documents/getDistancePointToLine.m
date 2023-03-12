function distance = getDistancePointToLine(q, p1, p2)
% Computes the distance from a point to a line.
% Function requires the point to test q = [x,y] and two
% points to define the line, p1 = [x1,y1] and p2 = [x2,y2].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intro to Robotics, ME170A/ECE181A, Spring 2009
% Joey Durham
% Created: April 1, 2009
% Modified: April 15, 2014 by Rush Patel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

q_x = q(1);
q_y = q(2);
[a,b,c] = getLineThroughTwoPoints(p1,p2);

% Short way to compute distance from point to line is using
% the following formula
distance = abs(a*q_x + b*q_y + c)/sqrt(a^2 + b^2);

% Alternative method is to find the orthogonal projection of q onto
% the line, and then find the distance from q to the projected point
%p_x = (b^2*q_x - a*b*q_y -a*c)/sqrt(a^2 + b^2);
%p_y = (-a*b*q_x + a^2*q_y -b*c)/sqrt(a^2 + b^2);

% Distance from point to line is then the distance
% from q to the projection of q on the line
%altDistance = dist(q,[p_x,P_y]);

% Uncomment the following lines to display debug plot
% figure;
% hold on
% axis([min(q_x,p_x)-5, max(q_x,p_x)+5, min(q_y,p_y)-5, max(q_y,p_y)+5]);
% plot(q_x,q_y,'ok');
% plot(p_x,p_y,'or');
% slope = -a/b;
% intercept = -c/b;
% refline(slope, intercept);
% axis([min(q_x,p_x)-5, max(q_x,p_x)+5, min(q_y,p_y)-5, max(q_y,p_y)+5]);
% axis square
% hold off;