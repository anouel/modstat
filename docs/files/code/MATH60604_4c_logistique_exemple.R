data(intention, package = "hecmodstat")
# Régression logistique
form <- "achat ~ sexe + age + revenu + educ + statut + fixation + emotion"
mod <- glm(form, data=intention, family=binomial(link="logit"))  
summary(mod)
car::Anova(mod, type = "3")
# Intervalles de confiance à 95% pour la cote (exp(beta))
exp(cbind(coef(mod), confint(mod)))
