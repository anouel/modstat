
data(bixicol, package = "hecmodstat")

mod1 <- lm(lognutilisateur ~ celcius, data = bixicol)
mod2 <- lm(lognutilisateur ~ farenheit, data = bixicol)
isTRUE(all.equal(resid(mod1), resid(mod2)))
mod3 <- lm(lognutilisateur ~ celcius + farenheit, data = bixicol)
summary(mod3)
# Un des paramètres n'est pas estimé
mod4 <- lm(lognutilisateur ~ celcius + rfarenheit, data = bixicol)
summary(mod4)
# Les deux variables ne sont pas significatives, 
# conditionel à l'inclusion de l'autre unité de température
car::vif(mod4)
car::avPlots(mod4)

data(simcolineaire, package = "hecmodstat")
cor(simcolineaire)
mod5 <- lm(Y ~ ., data = simcolineaire)
# Facteur d'inflation de la variance
car::vif(mod5)
