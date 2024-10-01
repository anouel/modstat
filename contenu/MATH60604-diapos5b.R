
library(ggplot2)
theme_set(theme_classic())
library(patchwork)
data(college, package = "hecmodstat")
mod <- lm(salaire ~ sexe + echelon + service, data = college)
# Corrélations nulles
cor(resid(mod), model.matrix(mod))[-1]
cor(resid(mod), fitted(mod))
# Moyenne des résidus nulle
mean(resid(mod))

# Diagramme des résidus versus valeurs ajustées (gauche), 
# et variable explicative `service` (droite) pour le modèle
#  avec les données `college`, L'ordonnée à l'origine et la pente sont nulles.
mod <- lm(salaire ~ sexe + echelon + service, data = college)
g1 <- ggplot(data = data.frame(yhat = fitted(mod), 
                         e = resid(mod)),
       mapping = aes(x = yhat, y = e)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, col = "grey") +
  labs(x = "valeurs ajustées", y = "résidus ordinaires")
g2 <- ggplot(data = data.frame(service = college$service, 
                         e = resid(mod)),
       mapping = aes(x = service, y = e)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, col = "grey") +
  labs(x = "années de service", y = "résidus ordinaires")
g1 + g2

## Démonstration numérique de l'invariance
modA <- lm(salaire ~ sexe + echelon + service, data = college)
modB <- lm(salaire ~ 0 + sexe + echelon + service, # Enlever l'ordonnée à l'origine
           data = college |> 
            dplyr::mutate(service = scale(service)), # Centrer-réduire une variable
           contrasts = list(echelon = contr.sum)) # changer la paramétrisation
head(model.matrix(modA), n = 3L)
head(model.matrix(modB), n = 3L)
# Invariance du modèle
isTRUE(all.equal(fitted(modA), fitted(modB)))


## Calcul des résidus
e <- resid(modA) # résidus ordinaires
r <- rstudent(modA)
# Diagramme quantile-quantile des
# résidus studentisés externes
car::qqPlot(modA, id = FALSE)

# Nombre d'observations
n <- nrow(modA$model)
# Nombre de coefficients du modèle
nbetas <- length(coef(modA))
## Calcul des effets leviers
levier <- hatvalues(modA)
# Quels points sont influents?
which(proj > 2*nbetas/n)
?influence.measures
## Distance de Cook
distCook <- cooks.distance(modA)            


## Coefficient de détermination
summary(modA)$r.squared
y <- college$salaire
cor(fitted(modA), y)^2
