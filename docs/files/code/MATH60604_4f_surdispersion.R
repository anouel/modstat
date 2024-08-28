# Ajustement du modèle Poisson
data(intention, package = "hecmodstat")
form <- "nachat ~ sexe + age + revenu + educ + statut + fixation + emotion"
m1 <- glm(form, data=intention, family=poisson)
# Modèle binomiale négative
# le paramètre theta=1/k (notation SAS)
m2 <- MASS::glm.nb(form, data=intention)
# la valeur-p du test H0:k=0 vs H1:k>0 
lrtstat <- 2*as.numeric(logLik(m2)-logLik(m1))
pchisq(lrtstat, df = 1, lower.tail = FALSE)/2
#rapport de déviance sur degrés de libertés
deviance(m2)/ df.residual(m2)
