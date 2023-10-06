function d=asym(pt)
% d=asym(pt)
% pt(3,v) contient les points témoins gauche, médiane et droit
% de v distributions
% d(1,v) est la différence des largeurs des parties gauches moins droite
% normalisées par la distance droite moins gauche
d=([1 -2 1]*pt)./([-1 0 1]*pt);