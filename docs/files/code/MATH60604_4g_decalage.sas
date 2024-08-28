/* Modèle de régression binomiale négative 
pour le nombre de décès sur les routes 
avec terme de décalage */
data accident;
set modstat.accident;
logpopn=log(popn);
run;

proc genmod data=accident;
class moment(ref="jour") annee(ref="2010");
model nmorts=moment annee / dist=negbin link=log 
	offset=logpopn type3 lrci;
run;
