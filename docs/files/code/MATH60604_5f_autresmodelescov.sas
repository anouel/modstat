data vengeance; 
set modstat.vengeance;
tcat=t;
run;


/* Régression linéaire avec structure autorégressive d'ordre 1 hétérogène pour les erreurs */
proc mixed data=vengeance method=reml;
class id tcat;
model vengeance = sexe age vc wom t / solution;
repeated tcat / subject=id type=arh(1) r=1 rcorr=1; 
run;

/* Régression linéaire avec variance non-structurée pour les erreurs  */
proc mixed data=vengeance method=reml;
class id tcat;
model vengeance = sexe age vc wom t / solution;
repeated tcat / subject=id type=un r=1 rcorr=1; 
run;

