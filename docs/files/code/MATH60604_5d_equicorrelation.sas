/* Régression avec structure d'équicorrélation pour les erreurs */
proc mixed data=modstat.vengeance method=reml;
class id;
model vengeance = sexe age vc wom t / solution;
repeated / subject=id type=cs r=1 rcorr=1; 
run;
