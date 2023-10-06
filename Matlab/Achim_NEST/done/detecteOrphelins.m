function [orphelin,pr]=detecteOrphelins(data,seuil)
% [orphelin,pr]=detecteOrphelins(data,seuil);
% data (ns,nv,nt) si nt>1, la détection de variables orphelines est faite
% sur chaque temps de mesure et les probabilités sont combinées
% une variable est détectée comme orpheline si sa régression multiple est
% non significative à p>seuil (défaut 0.3)
% pr donne la probabilité que chaque variable soit indépendante des autres
if nargin<2, seuil=0.05; end
[ns,nv,nt]=size(data);
pr=zeros(nv,nt);
db=ns-nv-1;
for t=1:nt
    R=corrcoef(data(:,:,1));
    B=1./diag(inv(R));
    R2=1-B;
    F=R2*db./(B*nv);
    pr(:,t)=1-fcdf(F,nv,db);
end
if nt>1
    pr=combineProb(pr);
end
orphelin=find(pr>seuil);