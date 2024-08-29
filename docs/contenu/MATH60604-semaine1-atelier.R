library(hecedsm)

# Rosen and Jerdee (1974)
## Créer un tableau de contingence (matrice 2x2) des décomptes
RJ_cont <- matrix(c(32L, 12L, 19L, 30L),
                     ncol = 2,
                     nrow = 2,
                     byrow = TRUE)
# Calculer la statistique de test
obs_stat <- chisq.test(x = RJ_cont,
                       correct = FALSE)


# Liu et and (2023)
data(LRMM23_S1, package = "hecedsm")
head(LRMM23_S1)
ttest <- t.test(appreciation ~ role,
                data = LRMM23_S1,
                var.equal = FALSE)

# Brucks and Levav (2022)
data(BL22_L, package = "hecedsm")
# Modèle de régression binomiale négative
negbin <- MASS::glm.nb(ncreative ~ cond,
             data = BL22_L)
# Extraire la statistique de test
negbin_Anova <- car::Anova(negbin, type = 3)
broom::tidy(negbin_Anova)
# Modèle alternatif - statistique de test-t 
# pour comparisons de deux moyennes (échantillons indépendants)
ttest_BL22 <- t.test(ncreative ~ cond,
             data = BL22_L, var.equal = TRUE)
