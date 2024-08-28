/* Tests d'indépendance pour tableaux de contingence 2x2 */

proc genmod data=modstat.affpol;
class parti sexe;
model nombre = sexe parti / dist=poisson link=log type3;
run;

