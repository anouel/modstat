/* Modèle de Poisson pour le nombre d'achats */

proc genmod data=modstat.intention;
class educ revenu;
model nachat=sexe age revenu educ statut
     fixation emotion / dist=poisson link=log lrci type3;
run;


/* Régression négative binomiale*/

proc genmod data=modstat.intention;
class educ revenu;
model nachat=sexe age revenu educ statut
     fixation emotion / dist=negbin link=log lrci type3;
run;

/* valeur-p du test du rapport de vraisemblance pour H0:k=0 vs H1:k>0 */

data valp;
valp=(1-CDF('CHISQ',23.08,1))/2;
run;
proc print data=valp;
run;

/* Régression log-linéaire (réponse log(1+nachat)) */

data temp;
set modstat.intention;
lognachat = log(1+nachat);
run;
 
proc genmod data=temp;
class educ revenu;
model lognachat=sexe age revenu educ statut
     fixation emotion / dist=normal link=identity lrci type3;
run;
/* AVERTISSEMENT: on ne peut utiliser les critères d'information pour comparer les modèles parce qu'ils n'ont pas la même réponse -  (1+nachat) versus nachat */
