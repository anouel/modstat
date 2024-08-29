
/* Exercice 2.5 (eolienne) */

data eolienne;
set modstat.eolienne;
invitesse = 1/vitesse;
run;

* Exercice 2.5 (a);
/* les options par défaut des graphiques quand
 "ods graphics on" est activé sont "plots=diagnostics" */
ods graphics on;
proc glm data=eolienne plots=fitplot residuals(smooth);
model production=vitesse / ss3 solution;
store modele1;
run;

proc glm data=eolienne plots=fitplot residuals(smooth);
model production=invitesse / ss3 solution;
output out=resid predicted=just r=reso rstudent=rstud;
store modele2;
run;

* Exercice 2.5 (b);
data nouvdonnees;
   input vitesse invvelo;
   datalines;
5 0.2
; 

proc plm restore=modele1;
score data=nouvdonnees out=prediction1 predicted lcl ucl;
run; 
proc plm restore=modele2;
score data=newdata out=prediction2 predicted lcl ucl;
run; 

proc print data=prediction1;
run;
proc print data=prediction2;
run;


* Exercice 2.5 (c);
proc glm data=eolienne noprint plots=none;
model production=vitesse / ss3 solution noint;
output out=pasdordonnee r=reso;
run;

proc means data=pasdordonnee mean;
var reso;
run;

* Exercice 2.5 (d);
/* Diagramme quantile-quantile */
* Créer une copie des données;
data qqdonnees;
set resid;
keep rstud;
run; 
* Trier par résidus studentisés (méthode du canif);
proc sort data=qqdonnees;
by rstud;
run;

data qqdonnees;
set qqdonnees nobs = n;
/* positions graphiques */
u = _N_ / (n + 1); 
/* Degrés de liberté = n-k-1
où k est le nombre de betas (2 pour régression linéaire simple) */
q = quantile("T", u, n - 3); 
drop u;
run;
 
proc sgplot data=qqdonnees noautolegend aspect=1;
scatter x=q y=rstud;
lineparm x=0 y=0 slope=1;
xaxis label="quantiles théoriques Student" grid; 
yaxis label="quantiles observés" grid;
run;


/* Exercice 2.6 */

data intention;
set modstat.intention;
if(revenu=1) then revenu1=1; else revenu1=0;
if(revenu=2) then revenu2=1; else revenu2=0;
run;

proc glm data=intention;
model intention=revenu1 revenu2  / ss3 solution;
run;

proc glm data=intention;
class revenu;
model intention=revenu / ss3 solution;
run;

proc glm data=intention;
model intention=revenu / ss3 solution;
run;

/* Exercice 2.7 */
proc glm data=modstat.automobile plots=diagnostics;
model autonomie=puissance / ss3 solution;
run;

proc glm data=modstat.automobile plots=diagnostics;
model autonomie=puissance puissance*puissance / ss3 solution;
run;

proc glm data=modstat.automobile plots=diagnostics;
model autonomie=puissance puissance*puissance
 puissance*puissance*puissance / ss3 solution;
run;

/* Exercice 2.8 */

proc glm data=modstat.intention;
class educ;
model intention=fixation educ / ss3 solution;
run;

proc glm data=modstat.intention;
class educ;
model intention=fixation educ fixation*educ/ ss3 solution;
run;

proc glm data=modstat.intention;
class educ revenu;
model intention=revenu educ fixation emotion statut age sexe/ ss3 solution;
run;

/* Exercice 2.9 */

data trafficaerien;
set modstat.trafficaerien;
cannee = annee-1949;
lnpassagers = log(passagers);
run;

proc glm data=trafficaerien plots=diagnostics;
model passagers=annee / ss3 solution;
run;

proc glm data=trafficaerien;
model passagers=cannee / ss3 solution;
run;

proc glm data=trafficaerien plots=diagnostics;
class mois;
model passagers=cannee mois / ss3 solution;
run;

proc glm data=trafficaerien plots=diagnostics;
class mois;
model lnpassagers=cannee mois / ss3 solution;
run;



