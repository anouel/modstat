/* Tableau 7.3 de Introduction to Categorical Data Analysis par Agresti
 Sondage de Wright State University School of Medicine et du United Health
 Services de Dayton, Ohio. Le sondage demande aux étudiant.e.s de dernière
 année du secondaire (12e) près de Dayton s'ils avaient déjà consommé de 
 l'alcool, des cigarettes ou de la marijuana.
*/
data drogue;
input alc $ cig $ mar $ decompte;
cards;
oui oui oui 911
oui oui non 538
oui non oui 44
oui non non 456
non oui oui 3
non oui non 43
non non oui 2
non non non 279
;
run;

proc genmod data=drogue;
class alc cig mar;
model decompte = alc cig mar alc|cig alc|mar cig|mar / dist=poisson link=log type3;
store sortie_modele;
run;
/* Prédiction est exp(betahat*x) */
proc plm restore=sortie_modele;
score data=drogue out=preds pred=pred / ilink;
run;

