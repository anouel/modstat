proc glm data=modstat.interaction;
class sexe(ref="0");
model intention=sexe fixation / ss3 solution;
run;

proc glm data=modstat.interaction;
class sexe(ref="0");
model intention=sexe|fixation / ss3 solution;
run;
