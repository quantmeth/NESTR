function [axes,Rr,L,Vb,crit]=RCFA(R,nf)
% [axes,Rr,L,Vb,crit]=RCFA(R,nf);
% reçoit une matrice de corrélations R(v,v) 
% et le nombre nf de dimensions avec lequel réduire la matrice par un paramètre de crête (ridge)
% appliqué à la variance propre des variables estimée par régression multyiple
% en sortie, axes est (v,nf)
% Rr est la matrice réduite optimale
% L est la valeur du paramètre et crit le critère de reproduction de la
% matrice réduite à partir des facteurs, étant donné la diagonale
% La diagonale de Rr est ainsi 1-L*Vb
% Cette méthode est due à Jung et Takane, 2007 (ou 2008?)
vb=1./diag(inv(R));
options=optimset('MaxFunEvals',5e4,'MaxIter',5e4,'TolFun',1e-5,'TolX',1e-5);
L=0.9;
[L,crit]=fminsearch(@(L) critRCFA(L,R,Vb,nf),L,options);   % défini ci-dessous
[~,Rr,axes]=critRCFA(L,R,Vb,nf);
end

function [crit,Rr,axes]=critRCFA(L,R,Vb,nf)
% [crit,Rr,axes]=critRCFA(L,R,Vb,nf);
% calcule Rr=R-diag(L.*Vb), en extrait nf facteurs, fait de ce modèle la matrice sigma
% et retourne comme critère non pas 1/2 trace[(Rr-sigma)^2] mais
% la somme des carrés des différences Rr-sigma
% sauf une valeur très grance si une communauté dépasse l'unité 
if L>=1
    crit=9e9;
    return
end    
Rr=R-diag(L.*Vb);
[F,s]=svdR(Rr,nf);
if ~isreal(s)
    keyboard;
end
axes=F*diag(sqrt(s));
if any(s<0) || any(sum(axes.^2,2)>1.0)
    crit=9e9;
    return
end
D=axes*axes'-Rr;
crit=D(:)'*D(:);
end