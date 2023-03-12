%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intro to Robotics, ME170A/ECE181A, Spring 2010
% Joey Durham
% May 21, 2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close all;

%define the 8 vertices of a cube
v = [0 0 0; 1 0 0; 1 1 0; 0 1 0; 0 0 1; 1 0 1; 1 1 1; 0 1 1]; 
%define the 6 faces in terms of the 8 vertices
f = [1 2 3 4; 5 6 7 8; 2 3 7 6; 1 4 8 5; 3 4 8 7; 1 2 6 5];
%set the color to 0 red, 0.3 green, and 0 blue for the 8 vertices

fvc = [0 .3 0; 0 .3 0; 0 .3 0; 0 .3 0; 0 .3 0; 0 .3 0; 0 .3 0; 0 .3 0;];

%set up the standard 3D view
view(3); axis square

%image the green translucent cube
patch('Vertices',v,'Faces',f,'FaceVertexCData',fvc,...
         'FaceColor','interp','EdgeColor','flat',...
          'FaceAlpha', 0.2)
      
%change the color to blue
fvc = [0 0 .6; 0 0 .6;0 0 .6;0 0 .6;0 0 .6;0 0 .6;0 0 .6;0 0 .6];

% scale and offset the cube to make a mini-cube inside
v = 0.5*v+[.25 .25 .25;.25 .25 .25;.25 .25 .25;...
        .25 .25 .25;.25 .25 .25;.25 .25 .25;.25 .25 .25;.25 .25 .25];

%image the blue translucent cube
patch('Vertices',v,'Faces',f,'FaceVertexCData',fvc,...
      'FaceColor','interp','EdgeColor','flat',...
      'FaceAlpha', 0.2)

tolerance = 0.001;

v = [1; 0; 0];
theta = pi;

Rtest = aatorm( v, theta );

[vtest, thetatest] = rmtoaa(Rtest);

if( (norm(v - vtest) < tolerance && abs(mod(abs(theta - thetatest),2*pi)) < tolerance) | ...
    (norm(v + vtest) < tolerance && abs(mod(abs(theta + thetatest),2*pi)) < tolerance) )
    disp('Test 1 passed')
else
    disp('*****Test 1 failed******')
    v
    vtest
    theta
    thetatest
end

v = [1; 0; 0];
theta = -pi;

Rtest = aatorm( v, theta );

[vtest, thetatest] = rmtoaa(Rtest);

if( (norm(v - vtest) < tolerance && abs(mod(abs(theta - thetatest),2*pi)) < tolerance) | ...
    (norm(v + vtest) < tolerance && abs(mod(abs(theta + thetatest),2*pi)) < tolerance) )
    disp('Test 2 passed')
else
    disp('*****Test 2 failed******')
    v
    vtest
    theta
    thetatest
end

v = [1; 0; 0];
theta = 0;

Rtest = aatorm( v, theta );

[vtest, thetatest] = rmtoaa(Rtest);

% This test is a little different, can't extract vector from identity
% rotation so angle and valid vector is all that's needed
if( (norm(vtest) - 1 < tolerance && abs(mod(abs(theta - thetatest),2*pi)) < tolerance) | ...
    (norm(vtest) - 1 < tolerance && abs(mod(abs(theta + thetatest),2*pi)) < tolerance) )
    disp('Test 3 passed')
else
    disp('*****Test 3 failed******')
    v
    vtest
    theta
    thetatest
end

v = [-1; 0; -1];
v = v/norm(v);
theta = -pi/6;

Rtest = aatorm( v, theta );

[vtest, thetatest] = rmtoaa(Rtest);

if( (norm(v - vtest) < tolerance && abs(mod(abs(theta - thetatest),2*pi)) < tolerance) | ...
    (norm(v + vtest) < tolerance && abs(mod(abs(theta + thetatest),2*pi)) < tolerance) )
    disp('Test 4 passed')
else
    disp('*****Test 4 failed******')
    v
    vtest
    theta
    thetatest
end

v = [-1; 10; -1];
v = v/norm(v);
theta = pi/6;

Rtest = aatorm( v, theta );

[vtest, thetatest] = rmtoaa(Rtest);

if( (norm(v - vtest) < tolerance && abs(mod(abs(theta - thetatest),2*pi)) < tolerance) | ...
    (norm(v + vtest) < tolerance && abs(mod(abs(theta + thetatest),2*pi)) < tolerance) )
    disp('Test 5 passed')
else
    disp('*****Test 5 failed******')
    v
    vtest
    theta
    thetatest
end

