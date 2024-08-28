proc genmod data=modstat.suede;
class annee(ref="1961") jour;
model accidents = limite annee jour / 
	 dist=poisson link=log type3 lrci;
run;

/* Format long -> format court */
proc transpose data=modstat.suede out=suede1 prefix=accidents;
    by jour;
    id annee;
    var accidents;
run;

proc transpose data=modstat.suede out=suede2 prefix=limite;
    by jour;
    id annee;
    var limite;
run;

data suede;
    merge suede1(drop=_name_) suede2(drop=_name_);
    by jour;
    limite = limite1962-limite1961;
    accidents = accidents1961 + accidents1962;
run;

proc genmod data=suede;
model accidents1962/accidents = limite / 
	dist=binomial link=logit type3 lrci;
run;

