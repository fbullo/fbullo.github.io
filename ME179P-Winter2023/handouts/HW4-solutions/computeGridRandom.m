function computeGridRandom(n)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Carlos Torres, May 1, 2011
% Updated by Patrick Therrien, May 13, 2015
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For n = 100 in the unit square X = [0, 1]2 in the plane 
% this code plots the following: 
% a random uniformly generated with MATLAB command rand
 
d = 2; l = n ^ (1/d);
%Pseudo Random Grid
Rand_P = rand(n,d);
% Note: Each of the columns 'd' represents an element of the coordinate pair. 

% plot single markers for each coordinate pair 
%               (Rand_P(index,1),Rand_P(index,1))
hold on;
for Index = 1: n;
    plot(Rand_P(Index,1),Rand_P(Index,2),'*r','LineWidth',1,'MarkerSize',3);
end
set (gca, 'YTick', 0:1/l:1); set (gca, 'XTick', 0:1/l:1);
grid on; hold off; title ('Pseudo-Random Grid')

end


