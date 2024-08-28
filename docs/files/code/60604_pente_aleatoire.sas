/* Modèle avec ordonnées à l'origine et pentes aléatoires */
proc mixed data=modstat.poussin;
class poussin regime;
model masse = regime temps / solution;
random intercept temps / type=vc subject=poussin(regime);
/* Covariance par défaut est VC (effets aléatoires non corrélés) */
run; 

/* Devrait-on inclure une corrélation entre b0 et b1? */
proc mixed data=modstat.poussin;
class poussin regime;
model masse = regime temps / solution;
random intercept temps / type=un subject=poussin(regime);
/* Composantes ordonnées dans l'ordre d'apparition dans la formule */
run; 

/* Pour comparer les modèles, on fait un test de 
rapport de vraisemblance avec H0: omega_{12}=0 */
data valp;
valp = 1-CDF("chisq", 4866.9-4803.8, 1);
run;

proc print data=valp;
var valp;
run;
/* Preuves écrasantes que la pente et l'ordonnée à l'origine sont corrélées
l'estimé de omega_{12} est négatif, la corrélation estimée est de -0.92! */


/* Plutôt qu'un effet fixe pour régime, 
on pourrait inclure un effet aléatoire.
En revanche les poussins sont emboîtés au sein de régime. 
Pas un problème pour l'ajustement tant que les identifiants sont uniques */
proc mixed data=modstat.poussin;
class poussin regime;
model masse = temps regime / outp=pred3 outpm=pred2;
/* outpm est la moyenne marginale (Xbeta), 
 outp est la moyenne conditionnelle (Xbeta+Zb) */
random intercept temps / type=un subject=poussin(regime);
/* poussin emboîté au sein de regime: la syntaxe est ``enfant(parent)`` */
random intercept / subject=regime solution;
/* on peut avoir plusieurs appels à "random" */
run; 

/* Regardons l'impact de l'effet aléatoire 
 pour ce faire, on ajuste une droite différente pour chaque individu
 effets fixes, à des fins d'illustration seulement!) */
proc mixed data=modstat.poussin;
class poussin regime;
model masse = regime poussin|temps / outpm = pred1;
run; 

/* Créer une variable 1...n pour pouvoir fusionner les base de données */
data pred1;
set pred1;
id = _N_;
pred1 = pred;
keep pred1 temps regime poussin id;
run;

data pred2;
set pred2;
id = _N_;
pred2 = pred;
keep pred2 temps regime poussin id;
run;

data pred3;
set pred3;
id = _N_;
pred3 = pred;
keep pred3 temps regime poussin id;
run;
/* Fusionner les bases de données, transformer de format large en format long*/
data pred;
merge pred1 pred2 pred3;
by id;
pred = pred1; type = "pas de mise en commun"; output;
pred = pred2; type = "mise en commun totale"; output;
pred = pred3; type = "mise en commun partielle"; output;
drop pred1 pred2 pred3;
run;

/* Diagramme (panneaux) des valeurs prédites pour quatre poussins
un pour chacun des régimes */
proc sgpanel data=pred(where=(poussin='18' OR poussin='24' OR poussin='33' OR poussin='44'));
panelby poussin;
series x=temps y=pred / group=type;
run;

