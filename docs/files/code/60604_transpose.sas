/* PREMIER EXAMPLE */

/* Transformer une base de donnée de format court à format long
https://stats.idre.ucla.edu/sas/modules/how-to-reshape-data-wide-to-long-using-proc-transpose
*/
proc print data=modstat.dentaire(where=(id LE 5));
run;

proc transpose data=modstat.dentaire
	out = dentaire_long(rename=(col1=dist)) /* Renommer la réponse */
	name = y;
var y1-y4; /* variables à empiler */
by id genre; /* variables à conserver, la base de donnée DOIT être ordonnée */
run;

data dentaire_long;
  set dentaire_long;
  time=input(substr(y, 2), 1.);
  drop _label_ y;
run; 

data dentaire;
set modstat.dentaire;
   dist=y1; age=8;  t=1; output;
   dist=y2; age=10; t=2; output;
   dist=y3; age=12; t=3; output;
   dist=y4; age=14; t=4; output;
   drop y1 y2 y3 y4;
run;



/* DEUXIEME EXEMPLE */

/* Avec proc tranpose */

data beattheblues;
set modstat.beattheblues;
id = _N_;  /* Créer une variable indicatrice */
run;

proc sort data=beattheblues;
by id medicaments duree traitement;
run;

proc transpose data=beattheblues out=beattheblues_long;
var idb0 idb2 idb4 idb6 idb8;
by id medicaments duree traitement;
run;

data beattheblues_long;
set beattheblues_long;
 mois=input(substr(_name_, 4), 3.);
  drop _label_ _name_;
  rename col1=idb;
run;

/* Concaténer les séries manuellement */
data beattheblues;
set beattheblues;
   idb=idb0; mois=0; t=1; output;
   idb=idb2; mois=2; t=2; output;
   idb=idb4; mois=4; t=3; output;
   idb=idb6; mois=6; t=4; output;
   idb=idb8; mois=8; t=5; output;
   drop idb0 idb2 idb4 idb6 idb8;
run;

/* Graphique spaghetti */
proc sgplot data=beattheblues;
series x=mois y=idb / group=id grouplc=traitement;
run;

