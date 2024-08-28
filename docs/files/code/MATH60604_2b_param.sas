proc glm data=modstat.bixicol;
ods select ParameterEstimates;
model lognutilisateur=celcius;
run;

proc glm data=modstat.bixicol;
ods select ParameterEstimates;
model lognutilisateur=farenheit;
run;

proc glm data=modstat.bixicol;
ods select ParameterEstimates;
model lognutilisateur=celcius farenheit;
run;
