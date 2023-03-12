function [linkPoly, nextLinkBase] = displaceLinkPoly( linkPolyRef, linkAngle, linkBase, nextLinkBaseRef )

linkPoly = linkPolyRef*[cos(linkAngle) sin(linkAngle); -sin(linkAngle) cos(linkAngle)] +...
    ones(size(linkPolyRef))*[linkBase(1) 0; 0 linkBase(2)];

nextLinkBase = nextLinkBaseRef*[cos(linkAngle) sin(linkAngle); -sin(linkAngle) cos(linkAngle)] + linkBase;