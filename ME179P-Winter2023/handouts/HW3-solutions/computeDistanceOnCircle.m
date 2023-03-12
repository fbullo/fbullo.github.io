function distcircle = computeDistanceOnCircle(theta1,theta2)
% computeDistanceOnCircle returns the distance between the angles theta1 and theta2

distcc = mod((theta2 - theta1), 2*pi);
distc = mod((theta1 - theta2), 2*pi);
distcircle = min([distc, distcc]);
