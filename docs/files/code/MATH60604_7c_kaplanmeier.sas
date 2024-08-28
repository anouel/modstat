/* Statistiques descriptives */ 
proc means data=modstat.cancersein;
var temps;
run;

proc freq data=modstat.cancersein;
tables mort repimmuno;
run;


/* Estimateur de Kaplan-Meier de la fonction de survie */
proc lifetest data=modstat.cancersein method=km plots=(s(cl)) ;
time temps*mort(0);
run;

proc lifetest data=modstat.allaitement method=km plots=(s(cl)) ;
time duree*delta(0);
run;

