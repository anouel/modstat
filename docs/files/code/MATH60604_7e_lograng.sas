/* Comparaison de courbes de survie */
proc lifetest data=modstat.cancersein method=km plots=(s) ;
time temps*mort(0);
strata repimmuno;
run;

/* Le test du log rang correspond au test du score
pour le modèle de risques proportionnels de Cox */
proc phreg data=modstat.cancersein;
model temps*mort(0)=repimmuno;
run;

