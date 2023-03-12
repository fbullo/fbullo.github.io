function distance = computeDistanceOnTorus( alpha1,alpha2,beta1,beta2 )
dist1 = computeDistanceOnCircle(alpha1,alpha2);
dist2 = computeDistanceOnCircle(beta1,beta2);

distance = sqrt(dist1^2+dist2^2);



end

