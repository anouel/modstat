
/* Statistiques descriptives et tableau de fréquence pour nachat */

proc means data=modstat.intention n mean var min max;
var nachat;
run;

proc freq data=modstat.intention;
table nachat;
run;

/* Modèle de Poisson pour le nombre d'achats */

proc genmod data=modstat.intention;
class educ revenu;
model nachat=sexe age revenu educ statut
     fixation emotion / dist=poisson link=log lrci type3;
run;


