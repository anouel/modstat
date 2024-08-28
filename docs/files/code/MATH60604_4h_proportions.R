# Régression logistique pour données binomiales
# taux de succès aux examens pratique de conduite en Grande-Bretagne
data(gbconduite, package = "hecmodstat")
gbconduite$taille <- factor(cut(gbconduite$total, c(0, 500, 1000, Inf)), 
                         labels = c("petit","moyen","grand"))
gbconduite$region <- relevel(factor(gbconduite$region), "London")

with(gbconduite, table(region, taille))
# Argument est succès, échec
mod_gbconduite <- glm(cbind(reussite, total-reussite) ~ sexe + region + taille,
          data=gbconduite, family=binomial(link="logit"))
summary(mod_gbconduite)
exp(cbind(coef(mod_gbconduite), confint(mod_gbconduite)))
# Prédiction du modèle logistique - multiplier par taille!
predlogistic <- fitted(mod_gbconduite) * gbconduite$total
# Pour une nouvelle base de données, utiliser
# predict(mod_gbconduite, newdata = gbconduite, type = "response")
# Cette sortie donnera la probabilité de succès
# pour obtenir le dénombrement, multiplier par le nombre d'essais


data(crash, package = "hecstatmod")
mod_crash <- glm(cbind(ndeath, popn-ndeath) ~ time + year, family=binomial, data=crash)
summary(mod_crash)
expit <- function(x){ 1/(1+exp(-x))}
# Estimated base rate per 100 000 inhabitants
# is probability of death ("success") times 10^5
expit(coef(mod_crash)[1])*1e5
