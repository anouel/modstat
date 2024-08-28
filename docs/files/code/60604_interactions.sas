/* Données assurance - interaction et catégories fusionnées */

data assurance; 
set modstat.assurance;
obese = (imc >= 30);
if(obese = 1 AND fumeur="oui") then obesefum="fumeur/obèse";
else if(obese = 0 AND fumeur = "oui") then obesefum = "fumeur/non-obèse";
else obesefum = "non-fumeur";
agem = age - 18;
run;

proc glm data=assurance;
class obese(ref="0") fumeur(ref="oui");
model frais = obese|fumeur|agem / solution ss3; /* obese fumeur age obese*fumeur obese*age fumeur*age obese*fumeur*age*/
run;



/* Calculer le test F pour modèles emboîtés à la mitaine */
/* Fusionner non-fumeurs (même pente, même ordonnée à l'origine) */

proc glm data=assurance;
	model frais=age|obese|fumeur / ss3 solution;
	ods select OverallANOVA;
run;

proc glm data=assurance ;
	class obesefum;
	model frais=age|obesefum / ss3 solution;
	ods select OverallANOVA;
run;

/* sTATISTIQUE F, calculée à la mitaine
Numérateur: différence de somme du carré des erreurs des deux modèles M0-M1
 divisée par le nombre de restrictions
Dénominateur: estimé de la variance, soit la somme du carré des erreurs du modèle M1 / (taille de l'échantillon)
*/
data valp;
valp=1-CDF('F',((27739495631-27733794686)/2)/20852477.207,2,1330);
run;
proc print data=valp;
run;

/* Même pente pour tous les groupes d'âge */
proc glm data=assurance;
class obese(ref="0") fumeur(ref="oui");
ods select OverallANOVA;
model frais = obese|fumeur|agem / solution ss3; /* obese fumeur age obese*fumeur obese*age fumeur*age obese*fumeur*age*/
run;

proc glm data=assurance;
class obese(ref="0") fumeur(ref="oui");
ods select OverallANOVA;
model frais = obese|fumeur agem / solution ss3; /* obese fumeur age obese*fumeur obese*age fumeur*age obese*fumeur*age*/
run;

data valp;
valp=1-CDF('F',(27740585144-27733794686)/3/20852477.207,3,1330);
run;
proc print data=valp;
run;


proc glm data=assurance;
class obesefum(ref="non-fumeur");
model frais = obesefum|agem / solution ss3;
run;

/* Exercice 2.8 */

proc glm data=modstat.intention;
class educ;
model intention=fixation educ / ss3 solution;
run;

proc glm data=modstat.intention;
class educ;
model intention=fixation educ fixation*educ/ ss3 solution;
run;

proc glm data=modstat.intention;
class educ revenu;
model intention=revenu educ fixation emotion statut age sexe/ ss3 solution;
run;



