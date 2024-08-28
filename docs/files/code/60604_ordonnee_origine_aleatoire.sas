proc sgplot data=modstat.masseporc;
series x=nsemaines y=masse / group=id;
run;

data masseporc;
set modstat.masseporc;
temps = nsemaines;
run;

proc mixed data=masseporc;
class temps;
model masse=nsemaines / solution;
repeated temps / subject = id;
run;	

/* Ordonnée à l'origine aléatoire */
proc mixed data=masseporc;
class temps;
*sauvegarder les solutions des effets aléatoires via "outpred";
model masse=nsemaines / solution outpred = pred; 
random intercept / subject = id v=1 solution ;
run;

/* Essayons d'ajuster un modèle AR(1) en plus */
proc mixed data=masseporc;
class temps id;
model masse=nsemaines / solution;
random intercept / subject = id v=1 solution;
repeated id / type=sp(pow)(nsemaines) r=1;
run;
/* L'algorithme ne converge pas vers un maximum global - 
pas capable d'estimer un effet AR(1) en plus de la structure 
d'équicorrélation induite par l'ordonnée à l'origine aléatoire */

 
title "Courbe de croissance des porcs";
proc sgplot data=pred;
   scatter x=nsemaines y=masse;
   series x=nsemaines y=pred / group=id;
run;

