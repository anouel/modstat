/* Prédiction pour les données mobilisations */
proc mixed data=modstat.mobilisation;
class idunite;
model mobilisation = sexe anciennete agegest nunite / solution;
random intercept / subject=idunite solution;
ods output Mixed.SolutionR=re;
run;

data nouvelledonnees; 
input nunite idunite employe anciennete sexe mobilisation agegest; 
cards; 
9 1 10 5 0 . 40 
9 101 1 5 0 . 40
; 
run; 

/* Concaténer  nouvelledonnees à mobilisation */
data mobilisation; 
set modstat.mobilisation  nouvelledonnees; 
run;

proc mixed data=mobilisation; 
class idunite; 
model mobilisation = sexe anciennete agegest nunite 
     / solution outp=prediction outpm=mean; 
random intercept / subject=idunite type=vc; 
run;

/* Conserver uniquement les deux nouvelles observations pour la prédiction */
data tempmean;
set mean;
if _N_ LE 1016 then delete;
run;

proc print data=tempmean noobs;
var idunite pred stderrpred;
run;


data temppred;
set prediction;
if _N_ LE 1016 then delete;
run;

proc print data=temppred noobs;
var idunite pred stderrpred;
run;
