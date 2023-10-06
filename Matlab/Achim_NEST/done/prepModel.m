function model=prepModel(R,nf)
% model=prepModel(R,nf);
% R (nv,nv,nt) contient nt pleines matrices de corrélations
% pour nt temps de mesures répétées
% nf est le nombre de dimensions à utiliser
% model sera (nv,nf,nt)
%
% POC : Vérification d'usage
if R(1,1,1)~=1
    error('Les matrices en entrée de prepModel ne doivent pas être déjà réduites');
end
% Sortir les dimensions
[nv,~,nt]=size(R);
% Calculer RCFA pour chaque matrice de corrélation
for k=1:nt
    R(:,:,k)=reduitRCFA(R(:,:,k),nf);
end
% Concaténer t p x p matrices en une p x (t * p) matrice
% Produire svd sur cette matrice et extraire le left singular vectors U
[axes,~]=svds(reshape(R,nv,nv*nt),nf);
% Vérifier sens des axes
f=find(sum(axes.^3)<0);
axes(:,f)=-axes(:,f);
% Extraire les axes en matrice (p * p) x k 
if nf==1
    AX=axes*axes';
    AX=AX(:);
else
    AX=zeros(nv*nv,nf);
    for k=1:nf
        T=axes(:,k)*axes(:,k)';
        AX(:,k)=T(:);
    end
end
model=zeros(nv,nf,nt);
for t=1:nt
    D=AX\reshape(R(:,:,t),nv*nv,1);
    model(:,:,t)=axes*diag(sqrt(D));
end