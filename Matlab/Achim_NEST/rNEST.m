function [axes,modelProb]=rNEST(data,graph)
% [axes,modelProb]=rNEST(data,graph);
% data(nS,nVar,nTime)
% the nTime dimension may be ignored if there is no repeated measure.
% graph, if present, indicates to produce the graphic outputs and is the
% common caption for all the graphs
% For each hypothesis on the number of required dimensions, a graph is
% produced for each measurement time, along with the combined probabilities
% If data has 3 dimensions, common normalised factors are extracted simultaneously
% for all times but separate communalities are used to generate the surrogate data for
% each measurement repetition
% Reduction of the correlation matrices is obtained through Regularized
% Common Factor Analysis (RCFA, Jung & Takane, 2007)
% Variables with no more than 10 different values are treated as ordinal in the
% simulations although the Pearson correlation coefficient is used for all pairs of variables
% axes(nVar,nFct,nTime) gives the set of loadings for the retained solution
% modelProb(nFact+,1) ou (nFact,nTime+1) is the empirical probability for all models tested
% when nTime>1, the last column of modelProb is the combined probablity
% Orphan variables are first detected and removed for data analysis, except that their loadings in the axis solution are
% set to 0.
% (c) André Achim, July 2016, December 2022, April 2023
nrep=1000;
nOrdin=10;  % variables with this number of different scores or fewer are considered ordinal
[orphelin,pr]=detecteOrphelins(data,0.05);
if ~isempty(orphelin)
    if numel(orphelin)==1
        fprintf('Une variable orpheline détectée; elle sera associée à des saturations de 0: #');
    else
        fprintf('%d variables orphelines détectées; elles seront associées à des saturations de 0: #',numel(orphelin));
    end
    fprintf(' %d',orphelin);fprintf('\n')
    fprintf(' %.2f',pr); fprintf('\n')
    data(:,orphelin,:)=[];
end
[Ns,nVar,nTime]=size(data);
ordinal=[];
ordLim={};
ordVal={};
typeEig=@eigPCA;  % défini-ci-dessous
grph=nargin>1;
for k=1:nVar
    d=unique(data(:,k,:));
    if numel(d)<=nOrdin
        ordinal=[ordinal k];
        ordLim{end+1}=limitesOrdinales(data(:,k,:),d); % défini ci-dessous
        ordVal{end+1}=d;
    end
end
axes=[];
R=zeros(nVar,nVar,nTime);
dat=zeros(Ns*nTime,nVar);
% EigVal=zeros(nVar,nrep);
d=0;
for t=1:nTime
    R(:,:,t)=corrcoef(data(:,:,t));
    dat(d+1:d+Ns,:)=data(:,:,t);
    d=d+Ns;
end
Rtout=corrcoef(dat);
[p,X2,df]=BartlettSphericity(Rtout,Ns);
if p>.05
    modelProb=p;    
    warning('Les données se comportent comme du bruit: X2(%d)=%.2f, p=%.4f',df,X2,p);
    return
end
Ev=zeros(nrep,nVar);
hndl=[];
rg=round([.01 .05 .5 .95 .99]*nrep); % used in graphs and for median in PA from rank after 1
maxFct=Lederman(nVar);
modelProb=ones(maxFct,nTime);
for k=1:maxFct
    if grph
        hndl(end+1)=figure;
        tiledlayout(nTime,1);   %,'TileSpacing','Compact'
        ti=['rNEST for the ' num2str(k) ' factor model'];
        if nTime>1
            ti=[ti ' at times ' num2str(1:nTime)];
        end
    end
    fprintf(' %d',k);
    model=prepModel(R,k);
    AXES=model;  % garder pour quand on aura identifié la solution probable
    for t=1:nTime
        EigDat=typeEig(R(:,:,t),k);
        % Rr=model(R(:,:,t),k);
        recipe=ajouteUnique(model(:,:,t)); % défini ci-dessous
        nr=size(recipe,1);
        for j=1:nrep
            dt=randn(Ns,nr)*recipe;
            for o=1:numel(ordinal)
                a=ordinal(o);
                dt(:,a)=replaceOrd(dt(:,a),ordLim{o},ordVal{o});  % défini ci-dessous
            end
            Ev(j,:)=typeEig(corrcoef(dt),k);
        end
        modelProb(k,t)=1+sum(Ev(:,k+1)>=EigDat(k+1));
        if grph
            Ev=sort(Ev);
            % hndl(end+1)=figure;
            nexttile;
            col=.6*[1 1 1];
            fill([1:nVar nVar:-1:1],[Ev(nrep,:) Ev(1,end:-1:1)],col);
            hold on
            fill([1:nVar nVar:-1:1],[Ev(rg(2),:) Ev(rg(4),end:-1:1)],.5*col);
            plot(Ev(rg(1:2:5),:)','k');
            plot(EigDat,'r');
            h=max(Ev(end,k+1)+.3,EigDat(k+1)); g=k+.85;
            b=0;d=k+1.15;
            plot([g d d g g],[b b h h b],'b:');
            text(d,h,[' ' num2str(modelProb(k,t)) '/' num2str(nrep+1)]);
            xlim([.9,nVar+.1]);
            if t==1
                title([graph 10 ti]);   % c'est quoi ce '10'? C'est un <CR>
            end
        end
        modelProb(k,t)=modelProb(k,t)/(nrep+1);
    end
    if nTime>1
        modelProb(k,t+1)=combineProb(modelProb(k,1:t));
        if grph
            title([' prob. combinées: ' num2str(modelProb(k,t+1))]);
        end
    end
    if isempty(axes) && modelProb(k,end)>.05
        axes=AXES;
    end
    if modelProb(k,end)>.2
        modelProb(k+1:end,:)=[];
        break;
    end
end
if grph
    for j=numel(hndl):-1:1
        figure(hndl(j));
    end
end
if ~isempty(orphelin)
    % no=numel(orphelin);
    s=size(axes);
    s(1)=1;
    z=zeros(s);
    for p=orphelin'
        ax=cat(1,axes(1:p-1,:,:),z);
        axes=cat(1,ax,axes(p:end,:,:));
    end
end
end

function recipe=ajouteUnique(axes)
v=sum(axes.^2,2);
if any(v>1)
    keyboard;
end
recipe=[axes,diag(sqrt(1-v))]';
end

function li=limitesOrdinales(dat,u)
n=numel(u);
N=numel(dat);
li=zeros(n+1,1);
li(1)=-inf;
for k=1:n
    li(k+1)=norminv(sum(dat<=u(k))/N);
end
end

function da=replaceOrd(dat,ordLim,ordVal)
da=zeros(numel(dat),1);
for k=1:numel(dat)
    da(k)=ordVal(sum(dat(k)>ordLim));
end
end

% function eig=eigRCFA(R,nf)
% % réduit la pleine matrice R par RFA pour nf facteurs
% % et en retourne toutes les valeurs propres dans l'ordre décroissant des valeurs absolues
% [~,Rr]=RCFA(R,nf);
% [~,eig]=svdR(Rr);
% end

function eig=eigPCA(R,varargin)
[~,eig]=svdR(R);
end

