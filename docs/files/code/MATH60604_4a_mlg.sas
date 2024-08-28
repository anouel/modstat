/* Ajustements de modèles linéaires avec genmod et glimmix */

proc genmod data=modstat.intention;
class educ revenu;
model intention=sexe age revenu educ statut
fixation emotion / dist=normal link=identity;
run;

proc glimmix data=modstat.intention;
class educ revenu; 
model intention=sexe age revenu educ statut 
  fixation emotion / solution dist=normal link=identity; 
run;

