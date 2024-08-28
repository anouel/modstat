proc iml;
/* Code SAS/IML par Rick Wicklin pour un test-t pour deux échantillons
Formules dans la documentation SAS: https://bit.ly/30rr2GV
*/
  start TTestH0(x, y, alpha=0.05);
n1 = nrow(x);    n2 = nrow(y);             /* tailles d'échantillon */
   meanX = mean(x); varX = var(x);            /* moyenne et variance de chaque groupe */
   meanY = mean(y); varY = var(y);
   DF = n1 + n2 - 2;
   /* estimateur non-biaisé de la variance conjointe: https://blogs.sas.com/content/iml/2020/06/29/pooled-variance.html */
   poolVar = ((n1-1)*varX + (n2-1)*varY) / DF;
   /* calculer la statistique de test, la valeur critique et la règle de décision du test */
   t = (meanX - meanY) / sqrt(poolVar*(1/n1 + 1/n2));
   t_crit = quantile('t', 1-alpha/2, DF); 
   RejectH0 = (abs(t) > t_crit);    /* valeurs binaires 0|1 pour test bilatéral */
   powerS = RejectH0[:]; 			/* taux moyen de rejet de H0 */
   return powerS;                           
finish;

/* Simuler des observations de deux groupes*/
start SimTTest(L);                     /* argument principal: liste                 */
   x = j(L$'n1', L$'B');               /* allouer espace pour groupe 1  */
   y = j(L$'n2', L$'B');               /* allouer espace pour groupe 2       */
   call randgen(x, 'Normal', 0,         1);   /* simuler X ~ N(0,1)                 */
   call randgen(y, 'Normal', L$'delta', 1);   /* simuler Y ~ N(delta,1)             */
   return TTestH0(x, y, L$'alpha');          /* compter le pourcentage de rejet de H0 sur B tests  */
finish;
 
/* ----- Programme principal ----- */
%let NumSamples = 1e5; /*Nombre de réplications */
%let n1 = 10; /* taille de l'échantillon 1*/
  %let n2 = 10;
L = [#'delta' = .,
  #'n1' = &n1,
  #'n2' = &n2,
  #'B'  =  &NumSamples,
  #'alpha' = 0.05];
  
  /* Créer une grille de valeurs de delta et modifier delta dans la liste à l'aide d'une boucle */
    delta = T(do(-2, 2, 0.1));
  power = j(nrow(delta),1,.); /* allouer l'espace pour le résultat de la simulation */
do i = 1 to nrow(delta); /* boucle: pour chaque delta, exécuter le programme */
   L$'delta' = delta[i];
   power[i] = SimTTest(L);
end; 

*print power;
create simulation from delta power;  /* spécifier les matrices à enregistrer */
append from delta power;             /* répéter le nom des matrices */
close;

data simulation; 					/* renommer les colonnes pour des étiquettes plus évocatrices */
set simulation;
rename col1=meandiff col2=power;
run;

title "Puissance du test-t pour deux échantillons";
title2 "Échantillons de lois N(0,1) et N(delta,1), n1=&n1, n2=&n2";
proc sgplot data=simulation noautolegend;
   series x=meandiff y=power;
   inset ("Nombre de réplications"="&NumSamples") / border;
   refline  0.05 0.1 / axis=y; 
   yaxis min=0 max=1 label="Puissance (1 - P[erreur de Type II])" grid;
   xaxis label="Différence de moyenne entre les deux populations" grid;
run;
QUIT;
