data assurance;
	set modstat.assurance;
	if(imc >= 30) then obese = 1;
	else obese = 0;
	if(obese = 1 & fumeur = "oui") then fumobese="fumeur/obese";
	else if(obese = 0 & fumeur = "oui") then fumobese ="fumeur/non-obese";
	else fumobese="non-fumeur";
run;

proc glm data=assurance;
class fumobese sexe region;
model frais = fumobese|imc age sexe region / solution ss3;
output out=residus predicted=vajustees 
    r=reso rstudent=rsc;
run;

/* Graphique des résidus ordinaires contre valeurs ajustées */
proc sgplot data=residus noautolegend;
scatter y=reso x=vajustees;
loess y=reso x=vajustees; 
xaxis label="valeurs ajustées";
yaxis label="résidus ordinaires";
run;

/* Résidus ordinaires contre indice de masse corporelle */
proc sgpanel data=residus noautolegend;
panelby fumeur / uniscale=row;
scatter y=reso x=imc;
loess y=reso x=imc; 
rowaxis label="résidus ordinaires";
colaxis label ="indice de masse corporelle";
run;

/* Résidus contres variables non incluse*/
proc sgpanel data=residus noautolegend;
panelby sexe / uniscale=row;
vbox  reso / category=enfant;
scatter  x=enfant y=reso / jitter transparency=0.6;
colaxis label="nombre d'enfants";
rowaxis label="résidus ordinaires"; 
run;



data residus;
set residus;
arsc = abs(rsc);
run;

/* Vérifier l'homoscédasticité à l'aide de graphiques
On couvre les tests pour l'égalité des variances
 plus tard dans le cours
*/
proc sgplot data=residus noautolegend;
scatter y=arsc x = vajustees;
loess y=arsc x = vajustees;
yaxis label = "|résidus studentisés externes|";
xaxis label = "valeurs ajustées";
run;


proc sgplot data=residus noautolegend;
vbox rsc / category=fumobese;
yaxis label = "résidus studentisés externes";
xaxis label = "statut fumeur/obésité";
run;

data residusqq; 
set residus;
keep rsc;
run;

proc sort data=residusqq; by rsc; run; /* 1 */
data residusqq;
set residusqq nobs=nobs;
pp = _N_  / (nobs + 1);
/* Calculer les quantiles des statistiques d'ordre
et appliquer la transformation inverse pour
obtenir un intervalle de confiance 95% approximatif*/
pplow = quantile("beta", 0.025, _N_, nobs + 1 - _N_);
pphigh = quantile("beta", 0.975, _N_, nobs + 1 - _N_);
q = quantile("t", pp, 1329);
qlow = quantile("t", pplow, 1329);
qhigh = quantile("t", pphigh, 1329);
qdet = rsc - q;
qdethigh = qhigh - q;
qdetlow = qlow - q;
run;
 
proc sgplot data=residusqq noautolegend; 
band x=q upper=qhigh lower=qlow / fill transparency=.5 legendlabel="pointwise confidence intervals";
scatter x=q y=rsc;
lineparm x=0 y=0 slope=1; 
xaxis label="quantiles théoriques Student" grid; 
yaxis label="résidus studentisés externes" grid;
run;

 
proc sgplot data=residusqq noautolegend; 
band x=q upper=qdethigh lower=qdetlow / fill transparency=.5 legendlabel="pointwise confidence intervals";
lineparm x=0 y=0 slope=0; 
scatter x=q y=qdet;
xaxis label="quantiles théoriques Student" grid; 
yaxis label="résidus studentisés externes (moins traîne)" grid;
run;

proc sgplot data=residus;
histogram rsc;
density rsc / type=kernel;
keylegend / position=bottom;
run;
proc univariate data=residus noprint;
qqplot rsc / normal(mu=est sigma=est l=2)
square;
run;

/* Corrélogramme */
data trafficaerien;
set modstat.trafficaerien;
lnpassagers = log(passagers);
run;

proc glm data=trafficaerien;
model lnpassagers = mois annee;
output out=trafficaerienresid r=reso;
run;

proc timeseries data=trafficaerienresid plots=(acf pacf);
var reso;
run;
