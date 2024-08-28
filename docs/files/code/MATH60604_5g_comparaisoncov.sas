/* Collecter manuellement les valeurs de AIC/BIC et de REMl -2ll des autres sorties */

 /* Calcul manuel de la valeur-p pour rho dans le modèle AR(1) - test du rapport de vraisemblance */

data valp;
pval=1-CDF('CHISQ',94.9,1);
run;
proc print data=valp;
run;
