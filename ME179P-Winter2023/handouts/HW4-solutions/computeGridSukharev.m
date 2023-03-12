function computeGridSukharev(n)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Carlos Torres, May 1, 2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% For n = 100 in the unit square X = [0, 1]2 in the plane 
% this code plots the following sample sets:  


% Sukharev Grid. Given:
d = 2; l = n ^ (1/d);

% set the ranges
i = 1:l;

% This could be done as a single mesh operation; however, this might be
% easier to follow. P := point
P_i = (1/(2*l)) * ((2*i)-1);
P_j = (1/(2*l)) * ((2*i)-1); %NOTE: P_j = P_i

% plot single markers for each coordinate pair (P_i,P_j)
hold on;
for Index_i=1:l
    for Index_j=1:l
        plot(P_i(Index_i),P_j(Index_j),'*b','LineWidth',1,'MarkerSize',3);
    end
end
% set the tick lines for the grid
set (gca, 'YTick', [0:1/l:1]); set (gca, 'XTick', [0:1/l:1]);
grid on; hold off; title ('Sukharev or Center Grid');

