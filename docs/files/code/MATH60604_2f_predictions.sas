data newdata;
   set modstat.intention(obs=1);
   do fixation=0 to 6;
      output;
   end;
run;

proc glm data=modstat.intention noprint;
class sexe educ revenu;
model intention= fixation emotion 
    sexe age revenu educ / ss3 solution;
store modelinfo; 
run;

proc plm restore=modelinfo; 
score data=newdata out=prediction predicted 
    lclm uclm lcl ucl; 
run;

proc sgplot data=prediction;
band x=fixation upper=ucl lower=lcl / 
        fill transparency=.5 
        legendlabel="individuelle";
band x=fixation upper=uclm lower=lclm / 
        fill transparency=.1 
        legendlabel="moyenne";
series x=fixation y=predicted / 
legendlabel="prédiction";
yaxis label="intention d'achat";
xaxis label="temps de fixation (en secondes)";
run;



/* Exemple plus réaliste avec les données "college" */

ods exclude all;
ods noresults;
proc glm data=modstat.college;
class domaine echelon sexe;
model salaire = domaine echelon service sexe;
store modele1;
run;

data nouvcollege;
length echelon $ 10; 
/* SAS tronque les noms de variables de plus de 8 caractères 
 Ici, je déclare "échelon" comme variable catégorielle ($) de longueur max 10.*/
   input annees domaine $ echelon $ service sexe $;
   datalines;
5 applique titulaire 3 homme
; 

proc plm restore=modele1;
score data=nouvcollege out=prediction predicted lcl ucl;
run; 
ods exclude none;
ods results;

proc print data=prediction;
var predicted lcl ucl;
run;

