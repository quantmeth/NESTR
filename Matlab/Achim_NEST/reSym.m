function [datTr,TI]=reSym(dat,exterieur)
% [datTr,TI]=reSym(dat,exterieur);
% dat (N,v)
% exterieur (1), une fraction (défaut: 0.05)
% ou un nombre de points à exclure de chaque côté
% si exterieur donné en négatif, les messages ne seront pas émis
% datTr(N,v) les données transformées s'il y a lieu
% TI{j} donne la transformatin inverse pour la variable j
%
% POC: 
% dat = [1:15;30:-1:16]'

c=0.5; % pour indices commençant à 1; c'est -0.5 pour indices commençant à 0
[N,v]=size(dat);
if nargin<2
    exterieur=0.05;
    msg=1;
else
    msg=exterieur>0;
    exterieur=abs(exterieur);
end
if exterieur>0.4 && exterieur<1
    error('Le paramètre "exterieur" doit être <=0.4 ou un entier')
end
if N*exterieur<3  % pour que les 2 points les plus extrêmes de chaque côté ne participent pas à Tg et Td
    exterieur=2;
end
if exterieur>=1
    exterieur=(exterieur+1-c)/N;
end
m0=mean(dat);
std0=std(dat);
dat=(dat-m0)./std0;
se=2/sqrt(N);
fractions=[exterieur,.5,1-exterieur];
A=interpoleRang(dat,fractions);
par=zeros(v,1);
datTr=dat;
TI{v}=[];
for j=1:v
    as=asym(A(:,j));
    TI{j}='T=V';      % transformation inerse (TI) de la variable V si pas transformée
    if abs(as)>se
        if as<0
            inverse=true;
            std0(j)=-std0(j);
            anc=-A([3 2 1],j);
            datTr(:,j)=-datTr(:,j);
        else
            inverse=false;
            anc=A(:,j);
        end
        if numel(unique(dat(:,j)))<=10  % variable considérée ordinale >>>> À REVOIR !!!!
            mi=min(dat(:,j));
            if sum(dat(:,j)==mi)>=N/2   % n'est symétrisable que si ==N/2
                datTr(datTr(:,j)>mi,j)=mi+1;
                if inverse
                    datTr(:,j)=mi+2-datTr(:,j);
                end
                par(j)=-2;
            else
                if log(A(1,j))>0
                    if msg
                        warning('reSym ne peut resymétriser cette distribution. Essayez une valeur plus petite de "exterieur".')
                    end
                    return
                end
            end
        end
        mi=min(datTr(:,j));
        anc=anc-mi;
        x=1e-6;
        dg=asym(log(anc+x));
        if dg>0  % on n'a apparemment pas de solution pour ce Tg
            if msg
                fprintf('\nLa distribution de la %d-ième variable ne peut être améliorée en excluant autant d''observations',j);
            end
            dt=sort(datTr(:,j))-mi;
            r=sum(dt<anc(1));
            for rr=r:-1:1
                anc(1)=dt(rr);
                anc(3)=dt(N+1-rr);
                dg=asym(log(anc+x));
                if dg<0
                    break;
                end
            end
            if dg<0
                if msg
                    fprintf('\nMais ça peut aller si on exclut %d cas de chaque côté',rr-1)
                end
                exterieur=(r-c)/N;
                xg=x;
            else
                if msg
                    fprintf('\nTransformation abandonnée pour la variable %d',j)
                end
                break
            end
        end
        x=1;
        d=asym(log(anc+x));
        if d>0 % la distribution est encore étirée à droite
            while d>0
                xd=x;
                dd=d;
                x=.5*x;
                d=asym(log(anc+x));
            end
            xg=x;
            dg=d;
        else
            while d<=0
                xg=x;
                dg=d;
                x=2*x;
                d=asym(log(anc+x));
            end
            xd=x;
            dd=d;
        end
        while abs(d)>.0001
            x=xg+(xd-xg)*dg/(dg-dd);
            d=asym(log(anc+x));
            if d>0
                xd=x;
                dd=d;
            else
                xg=x;
                dg=d;
            end
        end
        par(j)=x-mi;
        a=log(anc+x);
        ech=-norminv(exterieur);
        ech=ech/(a(2)-a(1));
        datTr(:,j)=(log(datTr(:,j)+par(j))-a(2)).*ech;
        if inverse
            datTr(:,j)=-datTr(:,j);
        end
        TI{j}=sprintf('%.5f*(exp(%.5f*V%+.5f)%+.5f)%+.5f',std0(j),1./ech,a(2),-par(j),m0(j));
        % TI{j}=sprintf('%.8f*(exp(%.8f*V%+.8f)%+.8f)%+.8f',std0(j),1./ech,a(2),-par(j),m0(j));
    end
end