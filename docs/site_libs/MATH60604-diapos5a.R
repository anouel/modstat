library(hecedsm)
library(hecmodstat)
library(ggplot2)
library(knitr)
theme_set(theme_classic())
library(patchwork)
library(emmeans)

# Nuages de points et droites ajustées pour un modèle avec une variable catégorielle
# binaire et une variable continue, sans (gauche) et avec (droite) un terme d'interaction.
data(interaction, package = "hecmodstat")
interaction <- interaction |>
  dplyr::mutate(sexe = factor(
    sexe,
    levels = c(0, 1),
    labels = c("homme", "femme")
  ))
mod <- lm(intention ~ fixation + sexe, data = interaction)
predmod <- predict(mod)
g1 <- ggplot(data = interaction,
             mapping = aes(x = fixation, y = intention, color = sexe)) +
  geom_point() +
  geom_line(aes(y = predmod), linewidth = 1, show.legend = FALSE) +
  MetBrewer::scale_color_met_d(name = "Hiroshige") +
  labs(color = "sexe", x = "temps de fixation (en secondes)", y = "intention d'achat")
g2 <- ggplot(data = interaction,
             mapping = aes(x = fixation, color = sexe, y = intention)) +
  geom_point() +
  geom_smooth(
    formula = y ~ x,
    se = FALSE,
    method = "lm",
    linewidth = 1,
    show.legend = FALSE
  ) +
  MetBrewer::scale_color_met_d(name = "Hiroshige") +
  labs(color = "sexe", x = "temps de fixation (en secondes)", y = "intention d'achat")
g1 + g2 + plot_layout(guides = 'collect') &
  theme(legend.position = "bottom")


# Pour spécifier une interaction, utiliser :
mod <- lm(intention ~ sexe + fixation +  sexe:fixation, data = interaction)
# Un raccourci est sexe*fixation, qui donne la même chose
# Vérifier la significativité de l'interaction
summary(mod)$coefficients



# Exemple 1
#
# Analyse des données de l'étude supplémentaire 5
# de Sharma, Tully et Cryder (2021)
data(STC21_SS5, package = "hecedsm")
# Check counts per subcategory (data are unbalanced)
xtabs( ~ purchase + debttype, data = STC21_SS5)
# On peut utiliser 'aov' pour ajuster des ANOVA
# Équivalent à lm, mais pas la même paramétrisation
aov_mod <- aov(likelihood ~ purchase * debttype, data = STC21_SS5)
# Calcul des moyennes globale/de lignes/de colonnes/de cellules
model.tables(x = aov_mod, type = "means")
# Ajuster l'ANOVA à deux facteurs 2x2 avec interaction
mod1 <- lm(likelihood ~ purchase * debttype, data = STC21_SS5)
# Extraire les moyennes marginales des quatre groupes
emm1 <- emmeans::emmeans(mod1, specs = c("debttype", "purchase"))


# Produire un diagramme d'interaction
emmeans::emmip(emm1, debttype ~ purchase, CIs = TRUE) +
  theme_classic() +
  theme(legend.position = "bottom")
# Aussi ?interaction.plot

# Tester si l'interaction est significativement différente de zéro
car::Anova(mod1, type = 2)
# Interaction pas significative, on peut se rabattre sur le modèle additif
# Calcul des moyennes marginales de "debttype" (effets principaux)
emmeans::emmeans(mod1, # mettre dans "specs" la variable à conserver
                 specs = "debttype", contr = "pairwise")

# On peut effectuer ces calculs en transformant notre ANOVA 2x2 à deux facteurs
# en ANOVA à un facteur avec quatre catégories
# Le calcul des contrastes est usuel
mod_ANOVA1 <- lm(likelihood ~ group, data = STC21_SS5 |>
                   dplyr::mutate(group = interaction(debttype, purchase)))
emmeans(mod_ANOVA1, specs = "group") |>
  contrast(method = list(main_pairwise = c(1, -1, 1, -1) / 2))


# Exemple 2
data(LKUK24_S4, package = "hecedsm")
xtabs( ~ politideo + chefdax + brandaction, data = LKUK24_S4)
# Ajuster le modèle avec toutes les interactions
mod2 <- lm(appropriation ~ politideo * chefdax * brandaction, data = LKUK24_S4)
# Extraire les moyennes marginales de chaque cellule
emm2 <- emmeans(mod2, specs = c("chefdax", "brandaction", "politideo"))
# Décider quelle variable associer à l'axe des x, couleur panneau
emmip(
  object = emm2,
  formula = brandaction ~ chefdax | politideo,
  CIs = TRUE
) # add 95% confidence intervals for mean

# Données pas balancées, utiliser la décomposition des carrés de type 2
# L'analyse de variance révèle que seule l'interaction idéologie/ethnicité est non-nulle
car::Anova(mod2, type = 2)

# Calcul des moyennes marginales par idéologie/chef Dax
# Calcul des effets simples par idéologie politique
(emm2a <- emmeans(mod2, specs = "chefdax", # variable à conserver
                  by = "politideo")) # variable sur laquelle on conditionne
# Calcul des différences 2 à 2 Follow-up with pairwise contrasts
emm2a |> contrast(method = "pairwise")

# Différence maintenant par ethnicité
emm2b <- emmeans(mod2, specs = "politideo", # variable à conserver
                 by = "chefdax") # variable sur laquelle on conditionne
emm2b
# Calcul des différences 2 à 2 Follow-up with pairwise contrasts
emm2b |> contrast(method = "pairwise")

# Effet marginal de brandaction
# Effet principal car pas d'interaction
(emm2c <- emmeans(mod2, specs = c("brandaction")))
# Test F global pour l'effet principal de brandaction
emm2c |> contrast(method = "pairwise") |> joint_tests()
# Remarque: les degrés de liberté du dénominateur sont toujours
# basés sur le modèle à trois facteurs avec interaction
# Cela évite le risque de mauvaise spécification
