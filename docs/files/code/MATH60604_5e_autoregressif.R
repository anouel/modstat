library(nlme)
data(vengeance, package = "hecmodstat")
vengeance$t <- as.integer(vengeance$temps)
modele0 <- gls(vengeance ~ sexe + age + vc + wom + t, 
              data = vengeance)
# Modèle autorégressif d'ordre 1
modele2 <- gls(vengeance ~ sexe + age + vc + wom + t,
              correlation= corAR1(form=~1|id),
              data = vengeance)
summary(modele2)
# Test de rapport de vraisemblance
anova(modele0, modele2)

# Matrices de covariance/corrélation pour individu 1
vcid1 <- getVarCov(modele2, individual = 1,
                   type = "marginal")
vcid1
cov2cor(vcid1)
