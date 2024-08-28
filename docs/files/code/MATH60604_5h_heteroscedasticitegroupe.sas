/* Hétéroscédascité de groupe */
proc mixed data=modstat.college plots=studentpanel;
class sexe domaine echelon;
model salaire = sexe domaine echelon;
repeated / group = echelon;
run;

