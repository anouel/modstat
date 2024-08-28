/* Créer une copie de 't' pour avoir une variable continue t pour la pente 
 et tcat comme variable catégorielle pour la commande repeated */

data vengeance; 
set modstat.vengeance;
tcat=t;
run;

/* Régression linéaire avec structure autorégressive d'ordre 1 pour les erreurs */
proc mixed data=vengeance method=reml;
class id tcat;
model vengeance = sexe age vc wom t / solution;
repeated tcat / subject=id type=ar(1) r=1 rcorr=1; 
run;
