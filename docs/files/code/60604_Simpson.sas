data admissionberk;
input departement $ sexe $ application admis rejete;
cards;
A homme 825 512 313
A femme 108 89 19
B homme 560 353 207
B femme 25 17 8
C homme 325 120  205
C femme 593 202 391
D homme 417 138 279
D femme 375 131 244
E homme 191 53 138
E femme 393 94 299
F homme 373 22 351
F femme 341 24 317
;
run;

/*Paradoxe de Simpson */
proc genmod data=admissionberk;
class sexe;
model admis/application = sexe/ dist=bin link=logit lrci type3;
run;


proc genmod data=admissionberk;
class sexe departement;
model admis/application = departement sexe / lrci noint dist=bin link=logit type3;
run;
