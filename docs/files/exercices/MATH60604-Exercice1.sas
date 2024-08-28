*1.1 (e) Histogramme du temps de parcours;
data renfe_tgv;
set modstat.renfe;
where type in ("AVE-TGV","AVE");
run;

proc sgplot data=renfe_tgv;
title1 "Histogramme du temps de parcours";
histogram duree;
label duree= "Temps de parcours";
run;

* 1.1 (e) Diagramme quantile-quantile;

proc univariate data=renfe_tgv noprint;
qqplot duree / normal(mu=est sigma=est color=red l=2) square ;
title "Diagramme quantile-quantile";
run;


*1.2 (i); Taux de couverture;
 
data renfe_couverture;
set modstat.renfe_simu;
if(icbi < -0.28 <icbs) then couvert=1; 
else couvert = 0;
run;

proc means data=renfe_couverture mean;
var couvert;
run;


*1.2 (b) Histogramme des différences moyennes;


proc sgplot data=modstat.renfe_simu noautolegend;
histogram difmoy;
refline -0.28 / axis=x lineattrs=(color=red pattern=dash);
label difmoy="Différence moyenne de prix (en euros)";
run;

*1.2 (c) Calcul de la puissance du test;

data renfe_puissance;
set modstat.renfe_simu;
if(valp < 0.05) then rejet=1;
 else rejet=0;
run;

proc means data=renfe_puissance mean;
var rejet;
run;

* 1.3 test-t pour un échantillon;

data renfe_avetgv;
set modstat.renfe;
where type="AVE-TGV";
drop type;
run;

proc ttest data=renfe_avetgv alpha=0.1 h0=43.25;
var prix;
title "Test-t pour un échantillon";
run;



/* 1.4(a):  Analyse exploratoire */

proc means data = modstat.assurance mean std min max q1 median q3 maxdec=2;
var frais imc;
run;
ods layout Start width=10in height=6in;
ods region x=0% y=0% width=50% height=33%;
ods graphics / noborder;
proc sgplot data = assurance;
histogram frais;
run;

ods region x=50% y=0% width=50% height=33%;
proc sgplot data = modstat.assurance;
histogram imc;
run;

/* Le graphique suivant montre 
clairement l'existence de regroupements et une traîne linéaire
au sein de chaque sous-groupe 
(ainsi que hétérogénéité de la variance) */
ods region x=0% y=33% width=50% height=33%;
proc sgplot data=modstat.assurance noautolegend;  
scatter y=frais x=age;
run;

ods region x=50% y=33% width=50% height=33%;
proc sgplot data=modstat.assurance noautolegend;  
scatter y=frais x=imc;
run;
ods region x=0% y=66% width=50% height=33%;
proc sgplot data=modstat.assurance;
vbox frais / category=region;
run;
ods region x=50% y=66% width=50% height=33%;
proc sgplot data=modstat.assurance;
vbox frais / category=fumeur;
run;
ods layout end;

proc sgplot data=modstat.assurance;
  title "frais (en dollars) en fonction de l'indice de masse corporelle pour les (non-)fumeurs.";
  scatter y=frais x=imc / group=fumeur;
run;

/* Trois regroupements visibles sur le nuage de points
  age versus frais - les fumeurs non-obèses et fumeurs-obèses
  paient davantage que les non-fumeurs*/

data assurance;
set modstat.assurance;
if (obese=1 & fumeur="oui") then fumobese=1; 
else if (obese=0 & fumeur="oui") then fumobese=2; 
else fumobese=3;
agemajo = age-18;
run;

proc glm data=assurance;
class fumobese region sexe;
model frais=agemajo fumobese region sexe / solution;
ods select ParameterEstimates;
run;


proc sgplot data=assurance;
  title "frais (en dollars) selon l'obésité et le statut de fumeur";
  scatter y=frais x=imc / group=fumeur;
run;


* 1.4(b) comparer les frais payés par les fumeurs et les non-fumeurs;
 
proc ttest data=assurance;
class fumeur;
var frais;
run;



* 1.4(c): comparer les frais médicaux entre fumeurs et non-fumeurs obèses;


data assurance_fumeur;
set assurance;
if fumeur=1;
run;

proc sgplot data=assurance_fumeur;
vbox frais/ category=obese;
run;

proc ttest data=assurance_fumeur sides=l alpha=0.05; 
/* changer la valeur de alpha
 pour le niveau de confiance*/
class obese;
var frais;
run;




