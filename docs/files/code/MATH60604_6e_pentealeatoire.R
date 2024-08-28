library(ggplot2)
library(lme4)
library(nlme)

data(vengeance, package = "hecmodstat")
vengeance$t <- as.integer(vengeance$temps)
mixte1 <- lmer(vengeance ~ sexe + age + vc + wom  +  t +
                 (1 | id) + (0 + t | id), data = vengeance, 
               REML = TRUE)

mixte2 <- lmer(vengeance ~ sexe + age + vc + wom +  t +
                (1 + t | id), data = vengeance,
               REML = TRUE)
# Même que 
# nlme::lme(vengeance ~ sexe + age + vc + wom  +  t, random = ~ 1+t | id, data = vengeance)

# Est ce que la pente et l'ordonnée à l'origine aléatoire sont corrélées
anova(mixte2, mixte1)
# Rejete fortement H0: corr=0
# pas un cas bordure, donc loi nulle asymptotique est khi-deux(1)
# Paramètres de la variance marginale 
VarCorr(mixte2) # imprime écarts-types et corrélation
VarCorr(mixte2)$id #imprime la matrice de covariance de B (effets aléatoires)

# Effets fixes 
fixef(mixte2) 
# Effets aléatoires
effets_aleatoires <- ranef(mixte2)
# Nuage de points des paires (pentes et ordonnées à l'origine aléatoires)
plot(effets_aleatoires)



# Parce que les covariables sont fixes dans le temps
# la combinaison des covariables revient à avoir une ordonnée à l'origine
# différente selon les caractéristiques

ordonnee_fixe <- c(cbind(1, as.matrix(vengeance[vengeance$temps==1,1:4])) %*% fixef(mixte2)[1:5])
pente_fixe <- nlme::fixed.effects(mixte2)["t"]


# Montre la droite ajustée pour le score de vengeance - échelle temporelle
ran_lines <- data.frame(intercept = ordonnee_fixe + effets_aleatoires$id[,1],
                        slope = pente_fixe + effets_aleatoires$id[,2],
                        id = 1:80)
ggplot(data = vengeance, aes(x=t, y=vengeance, group=id)) + 
         geom_line(alpha = 0.2) +
    geom_abline(data = ran_lines, 
                aes(intercept = intercept, slope = slope, group = id),  
                col = 4, alpha = 0.2) + 
  scale_x_continuous(expand = c(0,0), limits = c(1,5))


# Même modèle, cette fois-ci avec covariance AR(1) pour les aléas
# On doit utiliser nlme::lme plutôt que lme4::lmer
# puisque ce dernier ne permet que des matrices diagonales pour les aléas

mixte3 <- lme(vengeance ~ sexe + age + vc + wom + t, 
              random = ~ 1  | id, 
              data = vengeance, 
              correlation = corAR1()) 
mixte4 <- lme(vengeance ~ sexe +  vc + wom + age +  t, 
              random = ~ 1 + t | id, 
              data = vengeance, 
              correlation = corAR1()) 
# loi nulle est 0.5 chisq_1 + 0.5chisq_2 (pas à l'examen ;)
# mais loin de la réalité en échantillons finis
anova(mixte4, mixte3)
# avec le modèle AR(1), la pente aléatoire n'est pas nécessaire
plot(ranef(mixte3))
# Vérifions que la moyenne empirique des effets aléatoires est nulle
isTRUE(all.equal(0, mean(ranef(mixte3)[,1])))
nlme::plot.lme(mixte3)

