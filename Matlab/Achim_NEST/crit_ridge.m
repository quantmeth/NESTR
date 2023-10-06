function [crit,Rr,axes]=crit_ridge(L,R,Vb,nf)
% [crit,Rr,axes]=crit_ridge(L,R,Vb,nf);
% calcule Rr=R-diag(L.*Vb), en extrait nf facteurs, fait de ce modèle la matrice sigma
% et retourne comme critère 1/2 trace[(Rr-sigma)^2]
% % H est la matrice chapeau pour estimer les scores factoriels selon ce modèle
Rr=R-diag(L.*Vb);
[F,s]=svdR(Rr,nf);
axes=F*diag(sqrt(s));
if any(s<0) || any(sum(axes.^2,2)>1.0)
    crit=9e9;
    return
end
if s~=real(s)
    keyboard;
end
D=F*diag(s)*F'-Rr;
% crit=.5*trace(D^2);
% crit=sum(D(:).^2);
crit=D(:)'*D(:);
