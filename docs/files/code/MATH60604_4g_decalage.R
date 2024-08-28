# Modèle pour taux (dénombrement avec décalage)
data(accident, package = "hecmodstat")
# Fit an exponential-dispersion model 
mod <- MASS::glm.nb(nmorts ~ moment + annee + 
                      offset(log(popn)), data=accident)
# Taux de base 2010-jour
exp(coef(mod))  