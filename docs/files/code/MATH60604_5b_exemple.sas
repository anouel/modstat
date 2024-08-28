
 /* Évolution temporelle du désir de vengeance */

 /* Imprimer données des trois premiers individus */

proc print data=modstat.vengeance(where=(id<4));
run;


 /* Statistiques descriptives et corrélation */


proc means data=modstat.vengeance(where=(t=1));
var sexe age vc wom;
run;
proc corr data=modstat.vengeance(where=(t=1));
var sexe age vc wom;
run;


 /* Graphique spaghetti de vengeance */

proc sgplot data=modstat.vengeance;
series x=t y=vengeance / group=id LineAttrs= (pattern=1);
run;

 /* même graphique, mais seulement les dix premiers individus */

proc sgplot data=modstat.vengeance(where=(id<11));
series x=t y=vengeance / group=id LineAttrs= (pattern=1);
run;


