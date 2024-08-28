/* Alternatives pour l'ajustement d'un modèle logistique */

proc glimmix data=modstat.intention;
class educ revenu;
model achat(ref="0")=sexe age revenu educ statut 
    fixation emotion / dist=binary link=logit solution;
run;

proc genmod data=modstat.intention;
class educ revenu;
model achat(ref="0")=sexe age revenu educ statut 
    fixation emotion / dist=binomial link=logit lrci type3;
run;

proc logistic data=modstat.intention;
class educ revenu / param=glm;
model achat(ref="0")=sexe age revenu educ statut 
    fixation emotion / clparm=pl clodds=pl expb;
run;


/* Prédiction du modèle logistique */

proc logistic data=modstat.intention outmodel=binom_intention;
class educ revenu / param=glm;
model achat(ref="0")=sexe age revenu educ statut 
    fixation emotion / clparm=pl clodds=pl expb;
run;

proc logistic inmodel=binom_intention;
score clm data=modstat.intention out=pred;
/* fournir vos propres données pour des prédictions hors-échantillon */
run;

