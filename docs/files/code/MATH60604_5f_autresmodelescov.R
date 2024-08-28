library(nlme)
data(vengeance, package = "hecmodstat")
vengeance$t <- as.integer(vengeance$temps)
#ARH(1)
modele3 <- gls(vengeance ~ sexe + age + vc + wom + t,
              corr = corAR1(form = ~ 1 | id),
              weight = varIdent(form = ~ 1 | temps),
              data = vengeance)
summary(modele3)
# Covariance non structurée
modele4 <- gls(vengeance ~ sexe + age + vc + wom + t,
              correlation= corSymm(form=~1|id),
              data = vengeance)
summary(modele4)



# Matrices de covariance et corrélation pour individu 1
(vcid1 <- getVarCov(modele3, individual = 1,
                   type = "marginal"))
cov2cor(vcid1)
(vcid1 <- getVarCov(modele4, individual = 1,
                   type = "marginal"))
cov2cor(vcid1)
