library(nlme)
data(vengeance, package = "hecmodstat")
vengeance$t <- as.integer(vengeance$temps)
# Régression linéaire ajustée via REML
modele0 <- gls(vengeance ~ sexe + age + vc + wom + t, data = vengeance)
# Structure d'équicorrélation pour les erreurs intra-groupe
modele1 <- gls(vengeance ~ sexe + age + vc + wom + t,
              correlation= corCompSymm(form=~1|id),
              data = vengeance)
# Structure autorégressive d'ordre 1
modele2 <- gls(vengeance ~ sexe + age + vc + wom + t,
              correlation= corAR1(form=~1|id),
              data = vengeance)
# Structure autorégressive d'ordre 1 hétérogène
modele3 <- gls(vengeance ~ sexe + age + vc + wom + t,
              corr = corAR1(form = ~ 1 | id),
              weight = varIdent(form = ~ 1 | temps),
              data = vengeance)
# Structure de covariance non structurée
modele4 <- gls(vengeance ~ sexe + age + vc + wom + t,
              correlation= corSymm(form=~1|id),
              data = vengeance)
# tests de rapports de vraisemblance
anova(modele0, modele4) #indep versus non structurée
anova(modele1, modele4) #équisymmétrie vs non structurée
anova(modele2, modele4) #AR(1) vs non structurée
anova(modele2, modele3) #AR(1) vs ARH(1)
anova(modele3, modele4) #ARH(1) vs non structurée

# Critères d'information pour comparer tous les modèles
AIC <- AIC(modele0, modele1, modele2, modele3, modele4)
tab <- data.frame(
  modele = c("indep","équisymmétrie","ar(1)","arh(1)","non structurée"),
  df = AIC$df - 6L,
  AIC = AIC$AIC,
  BIC = BIC(modele0, modele1, modele2, modele3, modele4)$BIC)
print(tab)
