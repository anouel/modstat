/* Exercice 7.1 */
proc lifetest data=modstat.allaitement method=km plots=(s(cl));
time duree*censure(0);
strata fumeur;
run;

proc phreg data=modstat.allaitement;
model duree*censure(0) =  pauvrete fumeur scolarite agemere / ties=exact;
run;

/*  La statistique du log rang n'est pas identique, la faute aux duplicatas
La procédure lifetest utilise un ajustement différent (avec loi de référence hypergéométrique)
La procédure phreg utilise quant à elle la loi khi-deux de référence
Même conclusion, approximations différentes et références différentes */
proc phreg data=modstat.allaitement;
model duree*censure(0) = fumeur / ties=exact;
run;

/* Exercice 7.2 */
proc lifetest data=modstat.chaussures method=km plots=(s(cl));
/* On peut donner une liste de variables correspond 
à la censure à droite, séparées par une virgule*/
time temps*statut(1,2);
/* Exclure le tableau avec 6807 lignes de la sortie! */
ods exclude ProductLimitEstimates;
run;

proc phreg data=modstat.chaussures;
model temps*statut(1,2) = sexe prix / ties=exact;
run;

/* Ce code n'est pas relié à la question,
mais démontre comment tracer des courbes de survie
à partir du modèle de Cox */

/* Créer l'ensemble de variables explicatives 
pour lesquelles on veut tracer la fonction de survie */
data souliers;
input prix sexe;
datalines;
120 0
120 1
;
run;

/* Créer une fonction de survie (plots=survival) avec les covariables "covariates=in "covariates=shoes_prof"
save estimated survival in variable s of work.shoes_sp */
proc phreg data=modstat.chaussures plots(overlay)=survival;
model temps*statut(1,2)= sexe prix / ties=exact;
baseline out=souliers_sortie covariates=souliers survival=s; 
run;

