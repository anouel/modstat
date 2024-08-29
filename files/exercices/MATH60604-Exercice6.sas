/* Exercice 6.1 */
proc mixed data=modstat.goldstein method=reml;
class denom(ref="autre") type(ref="mixte") rv(ref="3");
model score = TLL rv sexe type denom / solution;
random intercept / subject=ecole v=37 vcorr=37 solution;
ods output Mixed.SolutionR=re;
run;

/* Diagramme quantile-quantile normal des effets aléatoires (prédictions) */
proc univariate data=re noprint;
qqplot estimate / normal(mu=est sigma=est l=2)
square;
run;

data re;
set re;
lcl = estimate - 1.96*StdErrPred;
ucl = estimate + 1.96*stderrpred;
run;

proc sort data=re;
by estimate;
run;

proc sgplot data=re;
scatter x=estimate y=subject / xerrorlower=lcl
xerrorupper=ucl
markerattrs=or;
 refline 0 / axis=x;
 xaxis label="effet aléatoire prédit avec intervalle de prédiction à 95%";
 yaxis label="école";
run; 


/* Classement des écoles, effets aléatoires + variables fixes au sein de groupes */

/* D'abord, trier les données et ne garder qu'une école chaque fois */
proc sort nodupkey data=modstat.goldstein out=pred_gold;
by ecole;
run;

/* Mettre à toutes les caractéristiques individuelles 
une valeur commune et ajouter une valeur manquante pour score
Add missing observations for score */
data pred_gold;
set pred_gold;
TLL = 0;
sexe = 0;
RV = 1;
score = .;
run;

/* Fusionner les deux bases de données */
data pred_gold;
set pred_gold modstat.goldstein;
run;

/* Réajuster le modèle et obtenir les prédictions conditionnelles*/
proc mixed data=pred_gold method=reml;
class denom(ref="autre") type(ref="mixte") rv(ref="3") ecole;
model score = TLL rv sexe type denom / solution outp=pred_gold_score;
random intercept / subject=ecole solution;
run;

/* Garder uniquement les données du classement */
data pred_gold_score;
set pred_gold_score(where=(score eq .));
run;
/* Trier les prédictions en ordre décroissant */
proc sort data=pred_gold_score;
by descending pred;
run;

/* Exercice 6.2 */
/* Modèle 1 */
proc mixed data=modstat.gsce;
class centre;
model resultat = score sexe / solution;
repeated / subject=centre type=cs;
run;

/* Modèle 2 */
proc mixed data=modstat.gsce;
class centre;
model resultat = resultat sexe centre / solution;
run;

/* Modèle 3 */
proc mixed data=modstat.gsce;
class centre;
model resultat =  score sexe / solution;
random intercept / subject=centre solution;
run;

