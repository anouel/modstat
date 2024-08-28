
 /* Régression linéaire sans modèle pour la corrélation intra-groupe 
 la méthode d'ajustement par défaut est reml, utiliser method=ml
  pour maximum de vraisemblance
 */

proc mixed data=modstat.vengeance method=reml;
model vengeance = sexe age vc wom t / solution;
run;
