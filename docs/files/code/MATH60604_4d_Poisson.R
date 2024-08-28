data(intention, package = "hecmodstat")
# Ajustement du modèle Poisson
form <- "nachat ~ sexe + age + revenu + educ + statut + fixation + emotion"
mod <- glm(form, data=intention, family=poisson)
# Tableau des coefficients avec tests de Wald
summary(mod)
# Effets de type 3 - tests du rapport de vraisemblance
car::Anova(mod, 3)
# Intervalles de confiance 95% basés sur la statistique du rapport de vraisemblance
confint(mod)
AIC(mod); BIC(mod); deviance(mod); df.residual(mod)
# La statistique du X2 de Pearson est obtenue
# en calculant la somme des résidus de Pearson.
sum(residuals(mod, type = "pearson")^2)
