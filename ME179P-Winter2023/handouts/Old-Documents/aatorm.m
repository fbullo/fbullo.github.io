function R = aatorm(k,theta)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intro to Robotics, ME170A/ECE181A, Spring 2011
% Anahita Mirtabatabaei
% June 4, 2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ensure k is a unit vector
k=k/norm(k);
khat = [0 -k(3) k(2)
        k(3) 0 -k(1)
        -k(2) k(1) 0 ];
if theta == 0 
    R = eye(3);
else
    R = eye(3) + sin(theta)*khat + (1-cos(theta))*khat^2;
%     R = k*transpose(k)+cos(theta)*[eye(3)-k*transpose(k)]+sin(theta)*khat;
end
