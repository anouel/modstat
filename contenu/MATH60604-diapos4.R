library(ggplot2)
theme_set(theme_classic())
library(patchwork)
library(emmeans) # estimations de moyennes marginales
library(hecedsm)
# Charger les données
data(SKD23_S2A, package = "hecedsm")
data(MV23_S1, package = "hecedsm")
data(BSJ92, package = "hecedsm")

# Modèle linéaire simple
lm_simple <- lm(pef ~ proportion, data = SKD23_S2A)
# Intervalles de confiance pour moyenne
prop_grille <- seq(0, 2, by = 0.1)
predci <- data.frame(cbind(
  proportion = prop_grille,
  predict(
    lm_simple,
    newdata = data.frame(proportion = prop_grille),
    interval = "c",
    level = 0.8
  )
))
# Intervalles de confiances pour prédictions
predpi <- data.frame(cbind(
  proportion = prop_grille,
  predict(
    lm_simple, # modèle
    newdata = data.frame(proportion = prop_grille),
    interval = "p", # choix, "p" est une abbréviation de "prédiction
    level = 0.8 # niveau du test (80% ici)
  )
))

# Calculer moyenne par groupe à proportion de 0, 0.5, 1 et 2
mean_prop <- SKD23_S2A |>
  dplyr::group_by(factor(proportion)) |>
  dplyr::summarize(mean = mean(pef)) |>
  dplyr::select(mean) |>
  unlist()
ggplot() +
  geom_point(
    data = SKD23_S2A,
    mapping = aes(x = proportion, y = pef),
    col = "grey",
    alpha = 0.8, # transparence
    position = position_jitter(width = 0.1, height = 0) # décaler observations
  ) +
  geom_segment(
    data = data.frame(x = c(0, 0.5, 1, 2), y = mean_prop),
    mapping = aes(
      x = x - 0.1,
      y = y,
      xend = x + 0.1
    ),
    linewidth = 1.5
  ) +
  geom_ribbon(
    data = predpi,
    aes(x = proportion, ymin = lwr, ymax = upr),
    fill = "grey70",
    alpha = 0.2
  ) +
  geom_ribbon(
    data = predci,
    aes(x = proportion, ymin = lwr, ymax = upr),
    fill = "grey70",
    alpha = 0.5
  ) +
  geom_abline(slope = coef(lm_simple)[2],
              intercept = coef(lm_simple)[1]) +
  scale_y_continuous(
    limits = c(1, 7),
    oob = scales::squish,
    breaks = 1:7,
    labels = 1:7
  ) +
  scale_x_continuous(
    limits = c(0, 2), 
    oob = scales::squish) +
  labs(x = "proportion de carton/plastique", 
       subtitle = "perception du respect de l'environnement", 
       y = "")


# Prédictions aux valeurs possibles
tab1 <- predict(lm_simple,
                newdata = data.frame(proportion = c(0, 0.5, 1, 2)),
                interval = "prediction") # intervalles de prédiction
tab2 <- predict(lm_simple,
                newdata = data.frame(proportion = c(0, 0.5, 1, 2)),
                interval = "confidence") # IC pour la moyenne
knitr::kable(
  cbind(proportion = c(0, 0.5, 1, 2), tab1),
  align = c("cccc"),
  col.names = c("`proportion`", "prédiction", "borne inf.", "borne sup."),
  booktabs = TRUE,
  caption = "Intervalles de prédiction"
)
knitr::kable(
  tab2,
  col.names = c("moyenne", "borne inf. (IC 95%)", "borne sup. (IC 95%)"),
  booktabs = TRUE,
  align = c("ccc"),
  caption = "Intervalles de confiance pour la moyenne"
)

######################
### Tests d'hypothèse
######################

# tests-t (Wald) pour beta=0 avec valeurs-p
summary(lm_simple)$coefficients
confint(lm_simple) # intervalles de confiance pour betas


# Tester les différences de moyenne
MV23_S1 <- MV23_S1 |>
  dplyr::mutate(amount2 = ifelse(is.na(amount), 0, amount))
mod_lin_MV23 <- lm(amount2 ~ condition, data = MV23_S1)
# Tests de Wald avec coefficients
summary(mod_lin_MV23)$coefficients
# Analyse de variance avec tests F
anova(mod_lin_MV23)
# Test-t pour deux échantillons
t.test(amount2 ~ condition, var.equal = TRUE, data = MV23_S1) 

# Comparer modèle linéaire et ANOVA
mod_lin <- lm(pef ~ proportion, data = SKD23_S2A)
coef(mod_lin) # extraire coefficients
# ANOVA à un facteur
mod_anova <- lm(pef ~ factor(proportion), data = SKD23_S2A)
# Comparer les deux modèles emboîtés
anova(mod_lin, mod_anova) # est-ce que l'effet est linéaire?
# Test avec code alternatif (poids pour chaque coefficient)
car::linearHypothesis(
  model = mod_anova,
  hypothesis = rbind(c(0, -2, 1, 0), 
                     c(0, 0, -2, 1)))

# Test global pour l'effet de groupe
mod_post <- lm(posttest1 ~ group + pretest1, data = BSJ92)
mod_post0 <- lm(posttest1 ~ pretest1, data = BSJ92)
anova(mod_post0, mod_post) # tests F

# Moyennes marginales
emmeans_post <- emmeans(object = mod_post, specs = "group")
knitr::kable(
  emmeans_post,
  digits = c(2, 2, 2, 0, 2, 2),
  booktabs = TRUE,
  col.names = c(
    "termes",
    "moyennes",
    "erreur-type",
    "ddl",
    "borne inf.",
    "borne sup."
  )
)

# Identifier l'ordre de niveau du facteur
with(BSJ92, levels(group))
# DR, DRTA, TA (alphabetical)
contrastes_list <- list(# Contrastes: combo linéaire de moyennes,
  # la somme des coefficients doit être nulle
  "C1: moy(DRTA+TA) vs DR" = c(-1, 0.5, 0.5),
  "C2: DRTA vs TA" = c(0, 1, -1))
contrastes_post <-
  contrast(object = emmeans_post, method = contrastes_list)
contrastes_summary_post <- summary(contrastes_post)


# Contrastes estimés pour le post-test 1.
knitr::kable(
  contrastes_post,
  booktabs = TRUE,
  digits = c(2, 2, 2, 0, 2, 2),
  col.names = c(
    "contraste",
    "estimation",
    "erreur-type",
    "ddl",
    "stat",
    "valeur-p"
  )
)


# Terme de décalage (ANCOVA vs ANOVA pour posttest1-pretest1)
# Extraire les coefficients et les erreurs-type
beta_pre <- coefficients(mod_post)['pretest1']
se_pre <- sqrt(c(vcov(mod_post)['pretest1', 'pretest1']))
wald <- (beta_pre - 1) / se_pre # test de Wald directionnel
# Valeur-p basée sur la référence nulle Student-t avec n-p-1 ddl
pval <- 2 * pt(abs(wald), df = mod_post$df.residual, lower.tail = FALSE)
# Comparaison de modèles emboîtés avec appel à 'anova'
mod0 <- lm(posttest1 ~ offset(pretest1) + group, data = BSJ92)
# Le décalage (`offset`) fixe le terme, ce qui équivaut à un coefficient de 1.
aov_tab <- anova(mod0, mod_post)


# ANOVA à un facteur
mod_anova <- lm(pef ~ factor(proportion), data = SKD23_S2A)
moy_marg <- mod_anova |>
  emmeans::emmeans(specs = "proportion") # moyennes de groupes
contrastes_list <- list(
  # liste de vecteurs de contrastes
  refvsdemi = c(1, -1, 0, 0),
  refvsun =  c(1, 0, -1, 0),
  refvsdeux =  c(1, 0, 0, -1)
)
# calculer différences relativement à la référence
contrastes <- moy_marg |> emmeans::contrast(method = contrastes_list)


## Moyennes estimées du PEF par proportion pour les groupes, avec erreurs-types
knitr::kable(
  moy_marg,
  digits = c(2, 2, 3, 0, 2, 4),
  booktabs = TRUE,
  col.names = c(
    "proportion",
    "moyenne",
    "erreur-type",
    "ddl",
    "borne inf.",
    "borne sup."
  )
) 


# Estimations des contrastes pour les différences de PEF relativement à plastique seulement.
knitr::kable(
  contrastes,
  booktabs = TRUE,
  digits = c(2, 2, 2, 0, 2, 2),
  col.names = c(
    "contraste",
    "estimation",
    "erreur-type",
    "ddl",
    "stat",
    "valeur-p"
  )
) 