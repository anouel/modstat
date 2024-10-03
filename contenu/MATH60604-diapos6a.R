## Chargement des paquets
library(ggplot2) # grammaire des graphiques
library(knitr) 
theme_set(theme_classic())
library(patchwork) # composition de graphiques
library(car) # compagnon à la régression appliquée
library(ggfortify) # extension de ggplot
library(nlme) # modèles mixtes et spécification de la variance
library(mgcv) # modèles généralisés additifs et lissage
library(qqplotr, warn.conflicts = FALSE) # diagrammes quantile-quantiles pour ggplot2
library(MASS)

## APPLICATION 1 - Modèle linéaire pour les données d'assurance
data(assurance, package = "hecmodstat")
# Créer une variable indicatrice pour l'obésité
assurance <- assurance |>
  dplyr::mutate(obesite = factor(imc >= 30, levels = c(FALSE, TRUE),
                                 labels = c("non-obese","obese")))
# Ajuster plusieurs modèles (mal spécifiés) de complexité croissante
modlin0_assurance <- lm(frais ~ age + obesite*fumeur, data = assurance)
modlin1_assurance <- lm(frais ~ age + obesite*fumeur*imc, data = assurance)
modlin2_assurance <- lm(frais ~ age + obesite*fumeur*imc + enfant, data = assurance)
modlin3_assurance <- lm(frais ~ splines::bs(age) + obesite*fumeur*imc + enfant, data = assurance)
# Le modèle 0 n'inclut pas l'interaction imc/fumeur et omet la variable enfant

## APPLICATION 2 - modèle linéaire pour les données de collège
data(college, package = "hecmodstat")
modlin1_college <- lm(salaire ~ echelon + domaine + sexe + service + annees, data = college)

## APPLICATION 3 - données de poisons
data(poisons, package = "SMPracticals")
modlin1_poisons <- lm(time ~ poison + treat, data = poisons)
modlin2_poisons<- lm(I(1/time) ~ poison + treat, data = poisons)

## APPLICATION 4 - traffic aérien
data(trafficaerien, package = "hecmodstat")
# Modèle avec indicateur mensuels et effet linéaire pour année
modlin1_traffic <- lm(passagers ~ factor(mois) + annee, data = trafficaerien)
# Idem, mais modèle log linéaire
modlin2_traffic <- lm(log(passagers) ~ factor(mois) + annee, data = trafficaerien)
# Modèle log linéaire avec base de Fourier pour les mois et effets nonlinéaire de l'année
modlin3_traffic <- lm(log(passagers) ~ I(cos(2*pi*mois/12)) +
                        I(sin(2*pi*mois/12)) +
                        I(cos(4*pi*mois/12)) +
                        I(sin(4*pi*mois/12)) +
                        I(cos(6*pi*mois/12)) +
                        I(sin(6*pi*mois/12)) +
                        I(annee) + I(annee^2),
                      data = trafficaerien)
# Modèle log-linéaire généralisé additif avec spécification flexible
modlin4_traffic <- mgcv::gam(
  log(passagers) ~ s(mois, bs = "cc") + s(annee),
  data = trafficaerien)

# Diagnostics graphiques
autoplot(modlin0_assurance, 1:6) # requiert ggfortify + ggplot2
plot(modlin0_assurance)

# Graphiques mnuels
assurance <- assurance |>
  dplyr::mutate(
    e0 = resid(modlin0_assurance),
    e1 = resid(modlin1_assurance),
    e2 = resid(modlin2_assurance),
    obese_fumeur = interaction(obesite, fumeur)
  )
g1 <- ggplot(data = assurance,
             mapping = aes(x = imc,
                           y = e0,
                           col = obese_fumeur,
                           fill = obese_fumeur)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = FALSE) +
  scale_color_viridis_d() + #palette de couleurs pour daltoniens
  scale_fill_viridis_d() +
  labs(x = "indice de masse corporelle",
       y = "résidus ordinaires",
       color = "obèse/fumeur",
       fill = "obèse/fumeur") +
  theme(legend.position = "bottom")
g2 <- ggplot(data = assurance,
       mapping = aes(x = enfant, y = e1)) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y ~ x, col = "black") +
  scale_color_viridis_d() +
  labs(x = "nombre d'enfants",
       y = "résidus ordinaires")
g1 + g2


## Linéarité: résidus

# Effet nonlinéaire de l'âge visible
car::residualPlots(modlin2_assurance, terms = ~ age)
# Le modèle avec des splines de lissage ou un effet quadratique capture la tendance
car::residualPlots(modlin3_assurance, terms = ~ splines::bs(age), test = FALSE)

# Pas de tendance, hormis l'hétéroscédasticité
# On voit clairement la variance augmenter avec le niveau
car::residualPlots(modlin1_college)

# Quelques effets résiduels dans la moyenne,
# très difficiles à capturer en l'état
car::residualPlots(modlin1_poisons)

# Variance augmente avec le niveau
trafficaerien <- trafficaerien |>
  dplyr::mutate(e3 = resid(modlin3_traffic),
                e4 = resid(modlin4_traffic))
car::residualPlots(modlin1_traffic)
car::residualPlots(modlin2_traffic)
# Le deuxième modèle est mieux que le premier, mais
# il y a des effets annuels résiduels
ggplot(data = trafficaerien,
       mapping = aes(x = mois, y = e3)) +
  geom_jitter()
ggplot(data = trafficaerien,
       mapping = aes(x = annee, y = e3)) +
  geom_jitter()
ggplot(data = trafficaerien,
       mapping = aes(x = mois, y = e4)) +
  geom_jitter()

ggplot(data = trafficaerien,
       mapping = aes(x = annee, y = e4)) +
  geom_point()


## Tests d'homogénéité de variance

# Extraire les résidus studentisés externes
r <- rstudent(modlin1_college)
# Test de Levene
car::leveneTest(r ~ echelon, center = "mean", data = college)
# Test de Bartlett (pas recommendé)
bartlett.test(r ~ echelon, data = college)
# Test de Breusch-Pagan (avec test du score)
# Ici, on choisit une seule variable explicative (par défaut, elles sont toutes incluses)
car::ncvTest(modlin1_college, var.formula =  ~ echelon)


# Ajuster le modèle par REML/moindres carrés généralisés
modlin_college2 <- nlme::gls(
  model = salaire ~ echelon + domaine + sexe + service, # spécification de la moyenne
  weights = nlme::varIdent(form = ~1 | echelon), # variance spécifique par échelon
  data = college)

# Graphique des résidus standardisés vs valeurs ajustées
plot(modlin_college2)
# Notez le changement dans les valeurs-p des tests de Wald
summary(modlin_college2)
# Le modèle est ajusté par maximum de vraisemblance restreint, alors les tests de rapport de vraisemblance ne sont pas identiques aux tests de Wald
anova(modlin_college2)

# Impact du sexe toujours pas significatif quand on prend en compte l'échelon

# Matrice de covariance sandwich
vcov_HCE <- car::hccm(modlin1_college)
# Tests de Wald avec matrice sandwich
w <- coef(modlin1_college) / sqrt(diag(vcov_HCE))
# Rapports de variance
diag(vcov_HCE) / diag(vcov(modlin1_college))
# Calcul des valeurs-p des tests de Wald avec estimateur sandwich
pval <- 2*pt(abs(w),
             df = modlin1_college$df.residual,
             lower.tail = FALSE)
# Aussi avec paquet "lmtest"
lmtest::coeftest(modlin1_college, vcov. = vcov_HCE)

library(forecast)
# Corrélogramme et autocorrélation partielle
acf(resid(modlin2_traffic))
pacf(resid(modlin2_traffic))
forecast::ggAcf(resid(modlin2_traffic),lag.max=25) +
  labs(x = "décalage", y ="autocorrélation", title = "") +
  ylim(c(-1,1))
# Ajuster un modèle en prenant compte l'autocorrélation (structure AR1)
modlin4_traffic <- nlme::gls(
  model = formula(modlin2_traffic),
  data = trafficaerien,
  correlation = nlme::corAR1())
# L'autocorrélation résiduelle est négligeable
forecast::ggAcf(resid(modlin4_traffic, type = "normalized"), lag.max=25) +
  labs(x = "décalage", y ="autocorrélation", title = "") +
  ylim(c(-1,1))


## Diagrammes quantiles-quantiles
car::qqPlot(modlin1_poisons, id = FALSE, ylab= 'résidus studentisés externes')
car::qqPlot(modlin2_poisons, id = FALSE, ylab= 'résidus studentisés externes')


## Transformation Box-Cox
car::boxCox(modlin1_poisons)
# Comparer modèle avec ou sans transformation
plot(modlin2_poisons, 1) 
car::qqPlot(modlin2_poisons)


## Régression robuste pour modèle d'assurance
rmod_ins <- MASS::rlm(
  data = assurance,
  frais ~ splines::bs(age) + obesite*fumeur*imc + enfant)
residualPlots(rmod_ins, tests = FALSE)
