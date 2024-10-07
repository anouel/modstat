data(bixicol, package = "hecmodstat")

modlin1_bixi <- lm(lognutilisateur ~ celcius, data = bixicol)
modlin2_bixi <- lm(lognutilisateur ~ farenheit, data = bixicol)
# Extraire les coefficients
beta_c <- as.numeric(coef(modlin1_bixi))
beta_f <- as.numeric(coef(modlin2_bixi))
# Vérifier la relation linéaire avec les paramètres
isTRUE(all.equal(beta_c, c(sum(c(1,32) * beta_f), 1.8*beta_f[2])))

# Modèle avec variables exactement colinéaires
modlin3 <- lm(lognutilisateur ~ celcius + farenheit, data = bixicol)
Xmat <- model.matrix(modlin3)
# La matrice a rang deux, mais trois colonnes.
eigen(t(Xmat) %*% Xmat, only.values = TRUE)$values
# La sortie du modèle inclut un avertissement
summary(modlin3)
# Un des paramètres n'est pas calculé


modlin4 <- lm(lognutilisateur ~ celcius + rfarenheit, data = bixicol)
summary(modlin4)
# Estimations, mais aucune de celles des températures n'est significative
# une fois que l'autre est prise en compte
car::vif(modlin4) # facteurs d'inflation de la variance
with(bixicoll, cor(celcius, rfarenheit)) # corrélation
car::avPlots(modlin4) # diagramme de corrélation partielle

## Exemple avec les données de college
data(college, package = "hecmodstat")
modlin1_college <- lm(salaire ~ echelon + sexe + service + domaine + annees, data = college)
summary(modlin1_college)
# Coefficients pour annees et service sont de signes opposés
car::vif(modlin1_college)
car::avPlots(modlin1_college, terms = ~ service + annees, id = FALSE)
