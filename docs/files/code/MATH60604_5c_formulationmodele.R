library(nlme)
data(vengeance, package = "hecmodstat")
vengeance$t <- as.integer(vengeance$temps)
# Modèle de régression linéaire ordinaire, ajusté via REML
modele0 <- gls(vengeance ~ sexe + age + vc + wom + t, 
              data = vengeance)
summary(modele0)
