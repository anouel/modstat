data(HBSC24_S4, package = "hecedsm")
# Statistiques descriptives de la variable 'authenticity'
HBSC24_S4 |>
  dplyr::group_by(cond) |>
  dplyr::summarize(mword = mean(authenticity),
                   sdword = sd(authenticity))
# Ajuster un modèle d'analyse de variance
mod1 <- lm(authenticity ~ cond, data = HBSC24_S4)
# Calculer les moyennes marginales par condition
emm <- emmeans::emmeans(mod1, specs = "cond")
# Définir une liste avec les poids des contrastes
cweights <- list(C1 = c(1,-0.5,-0.5), C2 = c(0, 1, -1))
# Obtenir un résumé des contrastes
emm |> emmeans::contrast(method = list(cweights))
# Note: les coefficients rapportés peuvent différer
# mais pas les statistiques de test ou les valeurs-p

# Tester pour l'impact de "authenticity" sur le nombre de mots "words"
summary(lm(words ~ authenticity, data = HBSC24_S4))$coefficients
