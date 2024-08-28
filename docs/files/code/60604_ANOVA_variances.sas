
/* Exemple avec ANOVA à un facteur */
proc mixed data=modstat.servqual;
class banque;
model fiabilite=banque / ddfm=satterth;
repeated / group=banque;
/*les tests-t pour chaque paire sont effectués à l'aide
de la statistique de Welch avec ajustement de Satterthwaite pour les DDL */
lsmeans banque / pdiff;
run; 

/* Analyse de variance à deux facteurs avec interaction 
Ici, on spécifie une variance différence pour chaque groupe */
proc mixed data=modstat.delai;
class stade delai;
model temps=stade|delai / ddfm=satterth;
repeated / group=stade * delai;
lsmeans stade*delai / pdiff;
run;
