# Exercice 2.5

data(eolienne, package = "hecmodstat")
lm_eol1 <- lm(production ~ vitesse,
               data = eolienne)
summary(lm_eol1)
lm_eol2 <- lm(production ~ I(1/vitesse),
               data = eolienne)
summary(lm_eol2)
# Bibliothèque graphiques
library(ggplot2)
library(patchwork)
g1 <- ggplot(data = eolienne, aes(x = vitesse, y = production)) +
    geom_point() +
    geom_smooth(formula = 'y ~ x', method='lm', se = FALSE) +
    labs(y = "production électrique",
         x = "vitesse du vent \n(en miles à l'heure)")
g2 <- ggplot(data = eolienne, aes(x = 1/vitesse, y = production)) +
    geom_point() +
    geom_smooth(formula = 'y ~ x', method='lm', se = FALSE) +
    labs(y = "production électrique",
         x = "vitesse du vent \n(en miles à l'heure)")
g1 + g2

g3 <- ggplot(data = data.frame(residuals = scale(resid(lm_eol1), scale = FALSE),
                               fitted = fitted(lm_eol1)),
             aes(x = fitted, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    labs(x = "valeurs ajustées",
         y = "résidus ordinaires",
         caption = "production ~ vitesse")
g4 <- ggplot(data = data.frame(residuals = scale(resid(lm_eol2), scale = FALSE),
                               fitted = fitted(lm_eol2)),
             aes(x = fitted, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    labs(x = "valeurs ajustées",
         y = "résidus ordinaires",
         caption = "production ~ 1/vitesse")
g3 + g4


# Quantile-quantile plots
library(qqplotr)
dfres <- lm_eol1$df.residual
g5 <- ggplot(data = data.frame(rstudent = rstudent(lm_eol1)),
             mapping = aes(sample = rstudent)) +
    stat_qq_band(distribution = "t", detrend = TRUE, dparams = list(df = dfres)) +
    stat_qq_line(distribution = "t", detrend = TRUE, dparams = list(df = dfres)) +
    stat_qq_point(distribution = "t", detrend = TRUE, dparams = list(df = dfres)) +
    labs(x = "quantiles théoriques",
         y = "quantiles empiriques",
         caption = "production ~ vitesse")
g6 <- ggplot(data = data.frame(rstudent = rstudent(lm_eol2)),
             mapping = aes(sample = rstudent)) +
    stat_qq_band(distribution = "t", detrend = TRUE, dparams = list(df = dfres)) +
    stat_qq_line(distribution = "t", detrend = TRUE, dparams = list(df = dfres)) +
    stat_qq_point(distribution = "t", detrend = TRUE, dparams = list(df = dfres)) +
    labs(x = "quantiles théoriques",
         y = "quantiles empiriques",
         caption = "production ~ vitesse")
g5 + g6

# Exercice 2.6

# Ce code montre comment déclarer des variables catégorielles
# url <- "https://lbelzile.bitbucket.io/MATH60604/intention.sas7bdat"
# intention <- haven::read_sas(url)
# intention$educ <- factor(intention$educ)
# intention$revenu <- factor(intention$revenu, ordered = FALSE)
# intention$revenu <- relevel(intention$revenu, ref = 3)
data(intention, package = "hecmodstat")


linmod1 <- coef(lm(intention ~ revenu, data = intention))
linmod2 <- coef(lm(intention ~ I(revenu == 1) + I(revenu == 2), data = intention))
linmod3 <- coef(lm(intention ~ as.numeric(revenu), data = intention))
# Print coefficients
linmod1
linmod2
linmod3

linmod4 <- lm(intention ~ fixation + emotion + revenu + educ + age + statut + sexe, data = intention)
summary(linmod4)
anova(linmod4)

# Exercice 2.7
data(automobile, package = "hecmodstat")

linmod1 <- lm(autonomie ~ puissance, data = automobile)
linmod2 <- lm(autonomie ~ puissance + I(puissance^2), data = automobile)
linmod3 <- lm(autonomie ~ puissance + I(puissance^2) + I(puissance^3), data = automobile)
hps <- seq(from = 40, to = 250, length.out = 100L)
p1 <- data.frame(x = hps,
                 predict(object = linmod1,
                         newdata = data.frame(puissance=hps),
                         interval = "prediction"))
p2 <- data.frame(x = hps,
                 predict(object = linmod2,
                         newdata = data.frame(puissance=hps),
                         interval = "prediction"))
p3 <- data.frame(x = hps,
                 predict(object = linmod3,
                         newdata = data.frame(puissance=hps),
                         interval = "prediction"))


g7 <- ggplot() +
    geom_point(data = automobile,
               aes(x=puissance, y = autonomie),
               alpha = 0.5) +
    geom_line(data = p1, aes(x = x, y = fit), col = "gray") +
    geom_ribbon(data = p1,
                aes(x = x, ymin = lwr, ymax = upr),
                col = 1, alpha = 0.1) +
    labs(x = "puissance (en chevaux vapeurs)",
         y = "autonomie \n(en miles au gallon)",
         caption = "linéaire")
g8 <- ggplot(linmod1) +
    geom_hline(yintercept = 0, col = "gray") +
    geom_point(aes(x=.fitted, y=.resid)) +
labs(x = "valeurs ajustées", y = "résidus ordinaires")
g9 <- ggplot() +
    geom_point(data = automobile,
               aes(x = puissance, y = autonomie),
               alpha = 0.5) +
    geom_line(data = p2, aes(x = x, y = fit), col = "gray") +
    geom_ribbon(data = p2,
                aes(x = x, ymin = lwr, ymax = upr),
                col = 1, alpha = 0.1) +
    labs(x = "puissance (en chevaux vapeurs)",
         y = "autonomie \n(en miles au gallon)",
         caption = "quadratique")
g10 <- ggplot(linmod2) +
    geom_hline(yintercept = 0, col = "gray") +
    geom_point(aes(x=.fitted, y=.resid)) +
labs(x = "valeurs ajustées", y = "résidus ordinaires")
g11 <- ggplot() +
    geom_point(data = automobile,
               aes(x=puissance, y = autonomie),
               alpha = 0.5) +
    geom_line(data = p3, aes(x = x, y = fit), col = "gray") +
    geom_ribbon(data = p3,
                aes(x = x, ymin = lwr, ymax = upr),
                col = 1, alpha = 0.1) +
    labs(x = "puissance (en chevaux vapeurs)",
         y = "autonomie \n(en miles au gallon)",
         caption = "cubique")
g12 <- ggplot(linmod3) +
    geom_hline(yintercept = 0, col = "gray") +
    geom_point(aes(x=.fitted, y=.resid)) +
labs(x = "valeurs ajustées", y = "résidus ordinaires")
(g7 + g8) / (g9 + g10) / (g11 + g12)

# test if the cubic term is significant
summary(linmod3)
# le modèle quadratique est aussi adéquat et plus simple
ggplot(data = data.frame(rstudent = rstudent(linmod2)),
       mapping = aes(sample = rstudent)) +
    stat_qq_band(distribution = "norm", detrend = TRUE) +
    stat_qq_line(distribution = "norm", detrend = TRUE) +
    stat_qq_point(distribution = "norm", detrend = TRUE,) +
    labs(x = "quantiles théoriques",
         y = "quantiles empiriques moins traîne théorique",
         caption = "quadratique")
# mais il n'explique pas tout- mauvais ajustement


# Exercice 2.8
data(intention, package = "hecmodstat")
lm_intent1 <- lm(intention ~ fixation + educ, data = intention)
lm_intent2 <- lm(intention ~ fixation * educ, data = intention)

#pdf("Exercice_2p8.pdf", width = 8, height = 4)
g1 <- ggplot(data = intention,
             aes(x = fixation, y = intention, col = educ)) +
    geom_line(data = lm_intent1, aes(y = .fitted), lwd = 1) +
    geom_point() + xlab("temps de fixation (en secondes)")
g2 <- ggplot(data = intention,
             aes(x = fixation, y = intention, col = educ)) +
    geom_line(data = lm_intent2, aes(y = .fitted), lwd = 1) +
    geom_point() + xlab("temps de fixation (en secondes)")
(g1 + g2) + plot_layout(guides='collect') &
    theme(legend.position='bottom')

#dev.off()



