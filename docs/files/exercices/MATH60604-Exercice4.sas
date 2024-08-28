/* Exercice 4.1 */

data salaireprof;
set modstat.salaireprof;
if(salaire > 105000) then salsup=1; else salsup=0;
run;

proc logistic data=salaireprof;
model salsup(ref="0") = diplome sexe anech andi / plrl;
run;

proc logistic data=salaireprof;
class echelon(ref="1");
model salsup(ref="0") = diplome sexe anech andi echelon / plrl;
run;

/* Exercice 4.2 */

proc genmod data=modstat.prix;
class prog(ref="1");
model nprix = prog math / dist=poisson link=log;
run;

proc genmod data=modstat.prix;
class prog(ref="1");
model nprix = prog math / dist=negbin link=log;
run;

data valp;
valp=(1-cdf('chisq', 1.6938, 1))/2;
run;
proc print data=valp;
run;

/* Exercice 4.3 */
data enfantsfiji;
set modstat.enfantsfiji;
lognfemmes = log(nfemmes);
menfants = nenfants/nfemmes;
run;

proc sgplot data=enfantsfiji;
scatter x=nfemmes y=nenfants;
yaxis type=log label="nombre d'enfants nés (log)";
xaxis type=log label="nombre de femmes (log)";
run;

proc sgplot data=enfantsfiji;
scatter x=menfants y=var;
xaxis label="moyenne du nombre d'enfants nés";
yaxis label="variance du nombre d'enfants nés";
run;

proc genmod data=enfantsfiji plots=(resdev(xbeta) leverage cooksd);
class res(ref="1") dur(ref="1") educ(ref="1");
model nenfants = res dur educ / 
	offset=lognfemmes dist=poisson link=log type3 lrci;
output out=residfiji stdresdev=devres;
run;

proc univariate data=residfiji noprint;
qqplot devres;
run;

proc genmod data=enfantsfiji;
class res(ref="1") dur(ref="1") educ(ref="1");
model nenfants = res dur educ lognfemmes / 
	dist=poisson link=log type3 lrci;
run;


proc genmod data=enfantsfiji;
class res(ref="1") dur(ref="1") educ(ref="1");
model nenfants = res dur educ / 
	offset=lognfemmes dist=poisson link=log type3;
run;

proc genmod data=enfantsfiji;
class res(ref="1") dur(ref="1") educ(ref="1");
model nenfants = res dur educ dur*educ / 
	offset=lognfemmes dist=poisson link=log type3;
run;

 /* Exercice 4.4 */
proc genmod data=modstat.bixi;
class fds;
model nutilisateurs = fds / type3 dist=poisson link=log;
run;

proc genmod data=modstat.bixi;
class fds;
model nutilisateurs = temp humid fds / type3 dist=poisson link=log;
run;

data valp;
valp = 1-CDF("chisq", 2726.9674- 1954.0018, 2);
run;
proc print data=valp;
var valp;
run;

proc genmod data=modstat.bixi;
class fds;
model nutilisateurs = temp humid fds / type3 dist=negbin link=log;
run;

proc genmod data=modstat.bixi;
class jour;
model nutilisateurs = temp humid jour / type3 dist=negbin link=log;
run;

/* Comparaison modèle avec indicateur pour 
chaque jour de semaine vs fin de semaine 
Test de rapport de vraisemblance */
data valp;
valp = 1-CDF("chisq", 522.3013 - 521.9627, 496-491);
run;
proc print data=valp;
var valp;
run;



/* Exercice 4.5 */
proc genmod data=modstat.socceragg;
class domicile visiteur;
model total= domicile visiteur / dist=poisson link=log;
run;

data valp;
valp = 1-cdf("chisq", 43.8008, 36);
run;

proc print data=valp;
var valp;
run;


proc genmod data=modstat.soccer;
class equipe adversaire;
model buts = equipe adversaire domicile / lrci type3 dist=poisson link=log;
store model_store;
run;

data nouvpartie;
length adversaire $25;
length equipe $25;
infile datalines delimiter=",";
input  domicile equipe adversaire;
datalines;
1, Manchester United, Liverpool
0, Liverpool, Manchester United
;
run;

proc plm source=model_store;
score data=nouvpartie out=pred pred=pred / ilink;
run;

proc print data=pred;
var pred;
run;
proc genmod data=modstat.soccer;
class equipe adversaire;
model buts = equipe adversaire domicile domicile*equipe domicile*adversaire / type3 dist=poisson link=log;
run;

data valp;
valp = 1-cdf("chisq", 2*(1082.6660-1058.7364), 720-682);
run;

proc print data=valp;
var valp;
run;

/* Exercice 4.6 */
data buch;
set modstat.buchanan(keep=buch totmb popn);
tot = buch + totmb;
buchp = buch/(buch + totmb);
lnpopn = log(popn);
run;

proc means data=buch;
var buchp;
run;

proc means data=buch sum;
var buch tot;
run;

proc sgplot data=buch;
scatter x=lnpopn y=buchp;
xaxis label="population du comté (log)";
yaxis label="pourcentage des suffrages exprimés pour Buchanan";
run;

data buchanan;
set modstat.buchanan;
lnhisp = log(hisp);
lncoll = log(coll);
if(comte="Palm Beach") then bucha=.; else bucha=buch;
lntotmb = log(totmb);
run;

proc genmod data=buchanan;
model bucha = blanc lnhisp a65 dsec lncoll revenu / offset=lntotmb dist=pois link=log;
run; 

proc genmod data=buchanan;
model bucha = blanc lnhisp a65 dsec lncoll revenu / offset=lntotmb dist=negbin link=log;
store model_store;		
run; 

data valp;
valp = (1-cdf("chisq", 2*(536.1785-328.8532), 1))/2;
run;

proc print data=valp;
var valp;
run;

proc plm source=model_store;
score data=buchanan out=preds pred=pred lclm=lower uclm=upper / ilink;
run;

data preds;
set preds(where=(comte="Palm Beach"));
keep comte pred lower upper;
run;

proc print data=preds;
var comte pred lower upper;
run;

