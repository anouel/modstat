proc glm data=modstat.bixicol;
model lognutilisateur = celcius / ss3;
run;

proc glm data=modstat.bixicol;
model lognutilisateur = farenheit / ss3;
run;

proc glm data=modstat.bixicol;
model lognutilisateur = celcius farenheit / ss3;
run;

proc glm data=modstat.bixicol;
model lognutilisateur = celcius rfarenheit / ss3;
run;

proc corr data=modstat.simcolineaire noprob;
var y x1-x5;
run;

proc reg data=modstat.simcolineaire; 
model y=x1-x5 / vif; 
run;

proc glm data=modstat.simcolineaire;
model y=x1-x5 / ss3 solution tolerance;
run;
