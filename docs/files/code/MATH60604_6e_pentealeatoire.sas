/* Pente aléatoire pour vengeance */

proc mixed data=modstat.vengeance;
class id;
model vengeance = sexe age vc wom t / solution;
random intercept t /  subject=id  type=un v=1 vcorr=1;
run;

/* Même modèle, mais avec des effets aléatoires non corrélés */
proc mixed data=modstat.vengeance;
class id;
model vengeance = sexe age vc wom t / solution;
random intercept t /  subject=id  v=1 vcorr=1; 
run;
