
LIBNAME multi "~/my_shared_file_links/u41148888/multi" access=readonly;

/* Le tableau des effets globaux pour les covariables (type 3)
sont basés sur le test de rapport de vraisemblance */
proc genmod data=modstat.massebebe;
class fumeuse hypertension irrit prematures;
model faible(ref="0")=fumeuse hypertension irrit prematures mmere
/ dist=binomial link=logit type3 lrci;
run;

/* La procédure logistique permet d'obtenir les paramètres
à l'échelle exp(beta) [cote] avec des intervalles de confiance 
obtenus à l'aide de la vraisemblance profilée */
proc logistic data=modstat.massebebe;
class fumeuse hypertension irrit prematures / param=glm;
model faible(ref="0")=fumeuse hypertension irrit prematures mmere / clodds=pl;
run;

/* Exercice:
1) interprétez l'effet des variables explicatives sur la cote
2) qu'arrive-t-il si vous changez la référence en ref="1" pour modéliser la probabilité d'avoir un bébé d'un poids normal?
3) ajoutez "mnaiss" à la liste des variables explicatives. Qu'arrive-t-il?
*/
