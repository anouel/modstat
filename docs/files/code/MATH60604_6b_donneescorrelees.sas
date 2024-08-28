
/* Données sur la mobilisation
proc mixed data=modstat.mobilisation method=reml; 
class idunite; 
model mobilisation = sexe anciennete agegest nunite / solution; 
repeated / subject=idunite type=cs r=1 rcorr=1; 
run;

