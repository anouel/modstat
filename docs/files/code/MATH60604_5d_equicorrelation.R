library(nlme)
data(vengeance, package = "hecmodstat")
vengeance$t <- as.integer(vengeance$temps)
modele0 <- gls(vengeance ~ sexe + age + vc + wom + t, 
              data = vengeance)
summary(modele0)

# Modèle avec structure d'équicorrélation pour les erreurs
modele1 <- gls(vengeance ~ sexe + age + vc + wom + t,
              correlation= corCompSymm(form=~1|id),
              data = vengeance)
# Tableau avec coefficients
summary(modele1)
# Tableau des effets de type III (tests de rapports de vraisemblance)
car::Anova(modele1, type=3)
# Rapport de vraisemblance pour comparer indépendence et équisymmétrie
anova(modele0, modele1)

# Matrice de covariance pour individu 1
vcid1 <- getVarCov(modele1, individual = 1,
          type = "marginal")
vcid1
cov2cor(vcid1)
