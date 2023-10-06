function [p,X2,df]=BartlettSphericity(R,N)
% [p,X2,df]=BartlettSphericity(R,N);
v=size(R,1);
X2=-((N-1)-(2*v-5)/6)*log(det(R));
df=v*(v-1)/2;
p=1-chi2cdf(X2,df);
% The formula for the chi-square value is: -( (n-1) - (2*p-5)/6 )* log(det(R))
% where n is the number of observations, p is the number of variables, and R is the correlation matrix.
% The chi square test is then performed on (p^2-p)/2 degrees of freedom.