/*Colinéarité */

/* Corrélation entre variables continues */
proc corr data = modstat.college;
var annees service;
run;
/* ici, R=0.9, donc R2 forcément supérieur à 0.81 pour années/service */
/* Pour tolérance, on regarde l'effet de type 2 (oubliez l'ordonnée à l'origine...)
TOL = 1-R2j, VIF = 1/TOL */
proc glm data=modstat.college;
class echelon sexe domaine;
model salaire = echelon sexe domaine annees service / ss3 solution tolerance;
run;

/* L'avertissement pour XtX pas invertible survient dès qu'on utilise l'option `class`
ou alors avec des variables continues parfaitement colinéaires */
proc glm data=modstat.bixicol;
model lognutilisateur = celcius farenheit / ss3 solution;
run;

/* On mesure education et age dans le sondage (questions faciles à répondre)
Par contre, sur le plan économique, le nombre d'années d'expérience est plus pertinent
/* ex = min(0, age - ed - 6). Une seule observation donne ex négatif */
/* procédure reg : vous devez coder les variable binaires manuellement */
proc reg data=statmod.wages85;
model lnwage = ed age ex exsq south nonwh hisp fe marr marrfe union / vif;
run;

/* les variables qui ont un VIF élevé ne sont pas problématiques, 
parce qu'on interprète ex et exsq (expérience au carré) conjointement (modèle quadratique)
idem pour l'interaction entre femme (fe) et marié (marr). */
proc reg data=statmod.wages85;
model lnwage = ed ex exsq south nonwh hisp fe marr marrfe union / vif;
run;

proc glm data=statmod.wages85;
model lnwage = ed age ex exsq south nonwh hisp fe marr marrfe union / ss3 tolerance;
run;
