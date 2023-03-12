
link1Poly = [-.1,-.2; 1.1,-.2; 1.1,.2; -.1,.2];
link2Poly = [-.1,-.1; 1.7,-.1; 1.7,.1; -.1,.1];

link1Base = [0,0];  % In absolute coordinates
link2Base = [1,0];  % In relative coordinates, should be inside link1Poly

figure;
hold on
[testPoly,testBase2] = displaceLinkPoly( link1Poly, 3*pi/4, link1Base, link2Base );
fill( testPoly(:,1), testPoly(:,2), 'b' )
[testPoly,testBase2] = displaceLinkPoly( link2Poly, 3*pi/4 - pi/2.5, testBase2, [0 0] );
fill( testPoly(:,1), testPoly(:,2), 'b' )

[testPoly,testBase2] = displaceLinkPoly( link1Poly, pi/4, link1Base, link2Base );
fill( testPoly(:,1), testPoly(:,2), 'g' )
[testPoly,testBase2] = displaceLinkPoly( link2Poly, pi/4 + pi/2.5, testBase2, [0 0] );
fill( testPoly(:,1), testPoly(:,2), 'g' )
hold off
axis([-3 3 -3 3])
axis square
