/* Exercice 5.1 */

proc print data=infe.mobilisation(where=(idunite<=4));
run;

 /* Modèle d'équicorrélation */
proc mixed data=infe.mobilisation method=reml;
class idunite;
model mobilisation = sexe anciennete agegest nunite / solution cl;
repeated / subject=idunite type=cs r=1 rcorr=1;
run;

proc mixed data=infe.mobilisation method=reml;
class idunite;
model mobilisation = sexe anciennete agegest nunite / solution cl;
run;
/* Modèle avec covariance non-structurée */
proc mixed data=infe.mobilisation method=reml;
class idunite;
model mobilisation = sexe anciennete agegest nunite / solution cl;
repeated / subject=idunite type=un r=1 rcorr=1;
run;

/* Exercice 5.3 */
/* Statistiques descriptives */
proc means data=infe.tolerance(where=(age=11));
var sexe exposition;
run;

proc means data=infe.tolerance;
var tolerance;
run;

proc sgplot data=infe.tolerance; 
title 'tolérance selon le sexe';
vbox tolerance/ category = sexe; 
run;

proc sgplot data=infe.tolerance; 
title 'tolérance versus exposition';
scatter y=tolerance x=exposition;
run;

proc sgplot data=infe.tolerance; 
title 'tolérance versus âge';
vbox tolerance/ category = age;
run;

proc sgplot data=infe.tolerance; 
title 'Graphique spaghetti de tolérance';
series x=age y=tolerance / group=id; 
run;

proc mixed data=infe.tolerance method=reml;
class id;
model tolerance = sexe exposition age / solution;
repeated / subject=id type=cs;	
run;

data tolerance;
set infe.tolerance;
temps = age;
run;

proc mixed data=tolerance method=reml;
class id temps;
model tolerance = sexe exposition age / solution;
repeated temps / subject=id type=ar(1);	
run;

proc mixed data=tolerance method=reml;
class id temps;
model tolerance = sexe exposition age / solution;
run;
