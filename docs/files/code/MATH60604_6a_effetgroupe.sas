/* Copier la variable temps */
data vengeance; 
set modstat.vengeance;
tcat=t;
run;

/* Pour comparer des modèles avec des  "effets fixes" (c'est-à-dire les betas)
 en utilisant les critères d'information, on doit utiliser la méthode du maximum de vraisemblance (method=ml)
  Ajuster un modèle avec effet groupe (pour chaque individu), temps et erreurs autorégressives d'ordre 1 */
proc mixed data=temp method=ml; 
class id; 
model vengeance = id sexe age vc wom t / solution; 
run;

proc mixed data=temp method=ml; 
class id tcat; 
model vengeance = id t / solution; 
repeated tcat / subject=id type=ar(1); 
run;

