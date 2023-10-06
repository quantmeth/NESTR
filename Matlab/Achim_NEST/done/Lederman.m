function maxF=Lederman(v)
% maxF=Lederman(v)
% retourne le nombre maximal de facteurs que v variables peuvent permettre d'identifier
maxF=floor((2*v+1-sqrt(8*v+1))/2);