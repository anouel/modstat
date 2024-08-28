/* Diagramme de régression partielle */
/* Régression de Y (salaire) sur X, mais sans service */
proc glm data=modstat.college;
class echelon(ref="adjoint") domaine(ref="applique") sexe(ref="homme");
model salaire=echelon domaine sexe / solution;
output out=ymodel r=residparty;
run;

/* Régression de service sur les autres X */
proc glm data=modstat.college;
class echelon(ref="adjoint") domaine(ref="applique") sexe(ref="homme");
model service=echelon domaine sexe / solution;
output out=xmodel residual=residpartx;
run;

/* Fusionner les bases de données et garder les résidus*/
data merged;
set ymodel(keep=residparty);
set xmodel(keep=residpartx);
run;

proc sgplot data=merged noautolegend;
scatter x=residpartx y=residparty;
reg x=residpartx y=residparty;
xaxis label="service | reste";
yaxis label="salaire | reste";
run;

proc glm data=modstat.college;
class echelon(ref="adjoint") domaine(ref="applique") sexe(ref="homme");
model salaire=echelon domaine sexe service / solution;
ods select ParameterEstimates;
run;

/* Vérifier que les coefficients pour service concorde et que l'ordonnée à l'origine est nulle */
/* Ce résultat découle du théorème de Frisch-Waugh-Lovell */
proc glm data=merged;
model residparty=residpartx;
ods select ParameterEstimates;
run;



