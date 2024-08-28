/* La procédure "transreg" ne permet pas la déclaration
de variables catégorielles avec "class" - il faut coder
manuellement toutes les variables binaires... */

data college;
set modstat.college;
if(echelon="aggrege") then echelon2=1; else echelon2=0;
if(echelon="titulaire") then echelon3=1; else echelon3=0;
if(domaine="theorique") then theorique=1; else theorique=0;
if(sexe="femme") then femme=1; else femme=0;
lnservice = log(service);
lnsalaire = log(salaire);
run;

data trafficaerien;
set modstat.trafficaerien;
if(mois=1) then mois1=1; else mois1=0;
if(mois=2) then mois2=1; else mois2=0;
if(mois=3) then mois3=1; else mois3=0;
if(mois=4) then mois4=1; else mois4=0;
if(mois=5) then mois5=1; else mois5=0;
if(mois=6) then mois6=1; else mois6=0;
if(mois=7) then mois7=1; else mois7=0;
if(mois=8) then mois8=1; else mois8=0;
if(mois=9) then mois9=1; else mois9=0;
if(mois=10) then mois10=1; else mois10=0;
if(mois=11) then mois11=1; else mois11=0;
lnpassagers = log(passagers);
run;

proc transreg data=trafficaerien plots=(boxcox obp);
model BoxCox(passagers / convenient lambda=-2 to 2 by 0.05) = 
	identity(mois1 mois2 mois3 mois4 mois5 mois6 mois7 mois8 mois9 mois10 mois11 annee);
run;

/* Le modèle est inadéquat et on pourrait l'améliorer
en ajoutant une interaction entre année et mois
au prix de 11 paramètres supplémentaires ... 
on aurait toujours un problème pour la variance */
proc glm data=trafficaerien plots=diagnostics;
class mois;
model passagers = mois annee / solution;
run;

proc glm data=trafficaerien plots=diagnostics;
class mois;
model passagers = mois|annee / solution;
run;


/* Moins d'artefacts avec ce modèle multiplicatif */
proc glm data=trafficaerien plots=diagnostics;
class mois;
model lnpassagers = mois annee / solution;
run;
/* Dans les deux cas, l'autocorrélation induit de la dépendance
les tests dans les sorties ne sont pas fiables (valeurs-p trop petites) */


/* Regarder les valeurs plausibles en s'attardant la vraisemblance 
profilée pour lambda de la transformée de Box-Cox*/
proc transreg data=college plots=(boxcox obp);
model BoxCox(salaire / convenient lambda=-2 to 2 by 0.05) = 
	identity(echelon2 echelon3 theorique femme service);
run;
/* ici, un modèle pour 1/y serait plus adéquat
 mais il n'aurait pas d'interprétation logique, donc à éliminer ... */

/* Comparaison du modèle pour log salaire versus salaire:
/* Un peu mieux, mais toujours trop d'hétéroscédasticité 
 L'interprétation changem, mais l'ajustement est grosso modo similaire*/
proc glm data=college plots=(diagnostics residuals);
model lnsalaire = echelon2 echelon3 theorique femme service / solution ss3;
run;

proc glm data=college plots=(diagnostics residuals);
model salaire = echelon2 echelon3 theorique femme service / solution ss3;
run;

