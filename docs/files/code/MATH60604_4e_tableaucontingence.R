data(affpol, package = "hecmodstat")
m0 <- glm(nombre ~ sexe + parti, 
          data=affpol, family = poisson(link="log"))
pchisq(deviance(m0), df = 2, lower.tail = FALSE)
# Il est inutile d'ajuster le modèle avec interaction
# c'est le modèle saturé, donc la statistique 
# du test pour l'interaction beta_interaction=0 
# est le même que la déviance
m1 <- glm(nombre ~ sexe * parti, 
          data=affpol, family = poisson(link="log"))
car::Anova(m1, 3)