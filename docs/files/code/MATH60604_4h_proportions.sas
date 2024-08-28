/* Modèle binomial pour les examens de conduite en Grande-Bretagne */

data gbconduite;
set modstat.gbconduite;
if(total < 500) then volume="petit"; 
else if (total < 1000) then volume="moyen";
else volume = "grand";
run;

proc tabulate data=gbconduite;
class volume region;
table region, volume;
run;

proc logistic data=gbconduite;
class sexe(ref="femme") region(ref="London") volume / param=glm;
model reussite/total = sexe region volume / link=logit clodds=pl expb;
run;

proc genmod data=gbconduite;
class sexe(ref="femme") region(ref="London") volume 
model reussite/total = sexe region volume / lrci dist=binomial link=logit type3;
run;

/* Modèle binomial pour le taux de décès */
proc logistic data=accident;
class moment(ref="jour") annee(ref="2010") / param=glm;
model nmorts/popn=moment annee / plcl plrl link=logit;
run;

