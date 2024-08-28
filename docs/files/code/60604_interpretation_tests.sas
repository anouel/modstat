/* Interprétation des tests pour les modèles linéaires avec les données assurance */
data assurance;
set modstat.assurance;
obese = (imc GE 30);
if (obese=1 & fumeur="oui") then fumobese="fumeur/obèse"; 
else if (obese=0 & fumeur="oui") then fumobese="fumeur/non-obèse"; 
else fumobese="non-fumeur";
age = age - 18;
fraism = frais;
run;

proc glm data=assurance;
class fumobese(ref="non-fumeur") region(ref="nordest") sexe;
model fraism=sexe region age fumobese / solution ss3 clparm;
run;

proc glm data=modstat.bixicol;
model lognutilisateur = celcius rfarenheit / ss3 solution;
run;

