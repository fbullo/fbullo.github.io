function [k, theta] = rmtoaa(R)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intro to Robotics, ME170A/ECE181A, Spring 2010
% Joey Durham
% May 21, 2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Check that R is a rotation matrix
% missing code here to check (1) dimension and (2) R R' - eye(3) is near zero

% Extract rotation angle from R
theta = acos( (trace(R) - 1)/2 );

k = [R(3,2) - R(2,3);
    R(1,3) - R(3,1);
    R(2,1) - R(1,2)];

if( norm(k) == 0 )
    k = [1; 0; 0];
end

k=k/norm(k);
