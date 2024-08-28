proc means data=modstat.intention mean std min max maxdec=2;
var intention sexe age statut fixation emotion;
run;

proc freq data=modstat.intention;
tables intention revenu educ;
run;

proc sgplot data=modstat.intention;
histogram intention emotion;
run;

proc sgplot data=modstat.intention noautolegend;
scatter y=intention x=fixation;
reg y=intention x=fixation;
yaxis label="intention d'achat";
xaxis label="temps de fixation (en secondes)";
run;

proc glm data=modstat.intention;
 *Imprimer seulement les coefficients;
ods select ParameterEstimates;
model intention=fixation;
run;

proc glm data=modstat.intention;
ods select ParameterEstimates;
model intention=sexe;
run;

/* Si pas codé avec 0/1, utiliser "class" */
proc glm data=modstat.intention;
class sexe(ref="0");
model intention=sexe / solution;
run;

data intention; 
set modstat.intention; 
educ1=(educ=1); 
educ2=(educ=2); 
run;

proc glm data=intention; 
ods select ParameterEstimates;
model intention=educ1 educ2; 
run;

 /* Alternative avec `class` */
proc glm data=modstat.intention; 
ods select ParameterEstimates;
class educ(ref="3"); 
model intention=educ / solution; 
run;
