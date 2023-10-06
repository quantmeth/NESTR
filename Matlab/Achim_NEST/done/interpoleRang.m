function val=interpoleRang(data,fractions)
% val=interpoleRang(data,fractions);
% retourne des valeurs dans l'échelle des variables de data (N,v) qui
% correspondent auz points centiles donnés par fractions(k)
% Par défaut, fractions = [.05 .5 .95]
% val sera (k,v)
c=0.5; % pour indices commençant à 1; c'est -0.5 pour indices commençant à 0
if nargin<2, fractions=[.05 .5 .95]; end
if fractions>=1
    fractions=max(.05,(fractions+1-c)/size(data,1));
    fractions=[fractions, .5, 1-fractions];
end
dat=sort(data);
f=fractions(:)*size(data,1)+c;
b=dat(floor(f),:);  % valeurs du bas
nb=multiplicite(dat,floor(f));     % multiplicité des valeurs du bas 
h=dat(ceil(f),:);   % valeurs du haut
nh=multiplicite(dat,ceil(f));     % multiplicité des valeurs du haut 
f=f-floor(f);     % la fraction
val=((1-f).*b.*nb+f.*h.*nh)*2./(nb+nh);
end

function nb=multiplicite(dat,i)
% nb=multiplicite(dat,i)
% dat(N,v)
% Pour chacun des k indices de rangée de dat donné dans i(k)
% retourne nb(k,v), la multiplicité des valeurs de dat= dat(i,:)
v=size(dat,2);
k=size(i,1);
nb=zeros(k,v);
for j=1:v
    for n=1:k
        nb(n,j)=sum(dat(:,j)==dat(i(n),j));
    end
end
end