function F=F4(k, R)
switch k
    case 0
        if nargin<2. R=.5; end
        if R>.75
            warning('La saturation du facteur doublet est réduite à 0.75.')
            R=0.75;
        end
        F=[ .6 .6 0 0 0
            .6 -.5 0 0 0
            .5 .6 0 0 0
            .5 -.5 0 0 0
            .5 .4 0 0 0
            .4 -.5 0 0 0
            .4 .4 R 0 0
            .4 -.3 R 0 0
            .4 .5 0 0 0
            .3 -.3 0 0 0
             0  0 0 1 0
             0  0 0 0 1];
        F(1:10,1:2)=F(1:10,1:2)+0.1;
%         F=[ .6 .6 0 0 0
%             .6 -.5 0 0 0
%             .5 .6 0 0 0
%             .5 -.5 0 0 0
%             .5 .4 0 0 0
%             .4 -.5 0 0 0
%             .4 .4 R 0 0
%             .4 -.3 R 0 0
%             .3 .4 0 0 0
%             .3 -.3 0 0 0
%              0  0 0 1 0
%              0  0 0 0 1];
        C=eye(5);
        C(1,2)=-.6;
        C(2,1)=-.6;
        F=F*C^.5;
        return
    case 1
        F=.1*[9 9 3 0 0 0 0 0 0 0 0 0
              0 0 0 7 7 4 0 0 0 0 0 0
              0 0 0 0 0 0 5 5 4 0 0 0
              0 0 0 0 0 0 0 0 0 9 7 5]';
    case 2
        F=[ 0.6   0  0.7   0
            0.6   0  0.7   0
            0.5 0.7    0   0
            0.7 0.5    0   0
            0.6   0  0.5   0
            0.5 0.5  0.5   0
              0 0.7  0.6   0
              0 0.5    0 0.8
              0   0  0.5 0.8
              0   0    0 0.8];
    case 3
        F=.1*[9 9 9 0 0 0 0 0 0 0 0 0
              0 0 0 8 8 8 0 0 0 0 0 0
              0 0 0 0 0 0 7 7 7 0 0 0
              0 0 0 0 0 0 0 0 0 6 6 6]';
    case 4
        t=1/sqrt(7);
        u=sqrt(.2);
        v=sqrt(.25);
        w=1/sqrt(3);
        F=[t t t t t t t 0 0 0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 u u u u u 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0 0 0 v v v v 0 0 0
           0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 w w w]';
    case 5    % cas 8 et 9 de Winter & Dodou 2012
        F=zeros(18,3);
        for k=1:3
            F(6*k-5:6*k,k)=.3;
        end
        F(1:2:end,:)=3*F(2:2:end,:);
    case 6   % pourrait désavantager l'analyse parallèle; vp: 4.860 4.335 3.840 0.5400
        F=zeros(24,4);
        for k=1:4
            F(6*k-5:6*k,k)=.95-.05*k;
        end
        F(F==.75)=.3;
    case 7  % Briggs&MacCallum2003 matrice 1
        F=[.95 .95 .95 .95 .95 0 0 0 0 0 0 0;
            0 0 0 0 0 .7 .7 .7 .7 0 0 0;
            0 0 0 0 0 0 0 0 0 .5 .5 .5]';
    case 8  % Briggs&MacCallum2003 matrice 2 "One-weak"
        F=[.95 .95 .95 .95 0 0 0 0 0 0 0 0 0 0 0 0;
            0 0 0 0 .85 .85 .85 .85 0 0 0 0 0 0 0 0;
            0 0 0 0 0 0 0 0 .8 .8 .8 .8 0 0 0 0;
            0 0 0 0 0 0 0 0 0 0 0 0 .45 .45 .45 .45]';
    case 9  % Briggs&MacCallum2003 matrice 3 "Two-weak"
        F=[.95 .95 .95 .95 0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0;
            0 0 0 0 .85 .85 .85 .85 0 0 0 0  0 0 0 0  0 0 0 0;
            0 0 0 0 0 0 0 0 0 0 0 0 .45 .45 .45 .45 0 0 0 0;
            0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 .4 .4 .4 .4]';
    case 10    % appelé F8 dans mes notes avant le 3 mars 2023; avec facteur doublet
        s8=sqrt(.8);
        s5=sqrt(.5);
        s3=sqrt(.3);
        F=[s8 s8 s8 s8  0  0  0  0  0;
             0  0  0  0 s5 s5 s5  0  0;
             0  0  0  0 0   0  0 s3 s3]';
    case 11   % appelé F9
        F=[F4(10);0 0 .5];
    case 12   % appelé F9C
        r=.5*ones(3)+diag(.5*ones(3,1));
        F=F4(11)*r^.5;
    case 13  % de Jung et Takane, 2007
       F=[.044 .899;.807 .061;.472 .063;.385 .691;.652 .207;
          .870 .236;.818 .188;.635 .193;.399 .737;.817 .186];
    case 14
       F=[.8 .8 .8 .8 0 0 0 0 0 0 0 0;
           0 0 0 0 .8 .63 .47 .3 0 0 0 0;
           0 0 0 0 0 0 0 0 .4 .4 .4 .4 ]';
end
if nargin>1
    nf=size(F,2);
    if numel(R)==1
        R=R*ones(nf,nf)+diag(ones(nf,1)-R);
    end
    F=F*R^.5;
end
        