proc glm data=modstat.intention;
ods select ParameterEstimates;
model intention=fixation / ss3 solution clparm;
run;

proc glm data=modstat.tickets;
ods select ParameterEstimates;
model offre=groupe / ss3 solution clparm;
run;

proc glm data=modstat.intention; 
class revenu educ sexe;
model intention=sexe age revenu educ statut fixation emotion age
   /ss3 solution; 
run;

