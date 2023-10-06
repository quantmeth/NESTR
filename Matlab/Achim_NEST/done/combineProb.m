function pr=combineProb(prob)
% prob est (f,t) des probabilités pour t temps de mesure
% pr sera (f,1) qui est la combinaison des t probabilités
g=size(prob,2);
if g==1
    pr=prob;
else
    z=sum(norminv(prob),2);
    pr=normcdf(z/sqrt(g));
end
end