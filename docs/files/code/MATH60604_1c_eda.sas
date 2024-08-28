proc sgplot data=modstat.renfe;
vbar classe / categoryorder=respdesc;
xaxis label="classe de train";
run;

data renfe_promo;
set modstat.renfe;
where tarif="Promo";
run;

proc sgplot data=renfe_promo noautolegend;
histogram prix;
density prix / type=kernel;
xaxis label="prix des billets au tarif Promo (en euros)";
run;

proc sgplot data=renfe_promo;
vbox prix / category=classe group=type;
yaxis label="prix (en euros)";
run;

data renfe_ave;
set modstat.renfe;
where type NE "REXPRESS";
run;

proc sgplot data=renfe_ave;
scatter y=prix x=duree / group=type;
xaxis label="durée de trajet (en minutes)";
yaxis label="prix (en euros)";
run;

