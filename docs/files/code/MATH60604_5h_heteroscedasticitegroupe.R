library(nlme)
data(college, package = "hecmodstat")
# Modèle linéaire avec erreurs homoscédastiques + indépendantes
modele1 <- gls(salaire/1000 ~ sexe + echelon + domaine, data = college)
# Modèle avec variance différente selon l'échelon, indépendantes
modele2 <- gls(salaire/1000 ~ sexe + echelon + domaine,
              weights = varIdent(form=~1 | echelon),
              data = college)
summary(modele2)
anova(modele1, modele2)
# Graphique des résidus standardisés versus valeurs ajustées
plot(modele2, xlab = "valeurs ajustées", ylab = "résidus standardisés")


# Analyse de variance à deux facteurs
# On peut aussi spécifier des variances pour des interactions
# entre variables catégorielles = variance pour chaque sous-catégorie
data(delai, package = "hecmodstat")

modele3 <- gls(temps ~ stade * delai, data = delai)
modele4 <- gls(temps ~ stade * delai, data = delai,
              weights = varIdent(form=~1 | stade *delai))
#Test de rapport de vraisemblance: variances égales?
anova(modele3, modele4)
# Mais l'interaction n'est pas significative
car::Anova(modele4, type=3)
