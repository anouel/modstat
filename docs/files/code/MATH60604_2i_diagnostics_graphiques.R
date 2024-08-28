library(ggplot2) #librairie graphique
library(patchwork) #combiner graphiques
library(dplyr) #manipuler base de données
library(viridis)

theme_set(theme_minimal())
options(ggplot2.continuous.colour="viridis")
options(ggplot2.continuous.fill = "viridis")
scale_colour_discrete <- scale_colour_viridis_d
scale_fill_discrete <- scale_fill_viridis_d

library(qqplotr, warn.conflicts = FALSE)


data(assurance, package = "hecmodstat")
assurance <- assurance %>%
  mutate(obese = factor(imc >= 30, 
                         labels = c("non-obese","obese")),
          fumobese = case_when(
            obese=="obese" & fumeur=="oui" ~ "fumeur/obese",
            obese=="obese" & fumeur=="non" ~ "non-fumeur/obese",
            obese=="non-obese" ~ "non-obese"),
          fumobese = factor(fumobese, 
                            levels = c("fumeur/obese","non-fumeur/obese","non-obese")),
         )

mod <- lm(frais ~ fumeur*obese*imc + age + sexe + region,
          data = assurance)
assur_lm <- assurance %>% 
  mutate(resid = resid(mod),
         ajustee = fitted(mod),
         rstud = rstudent(mod))
g1 <- ggplot(data = assur_lm, 
             aes(x = ajustee, y = resid)) + 
  geom_point(aes(col = fumobese)) +
  geom_smooth() +
  theme(legend.position = "bottom") + 
  ylab("résidus ordinaires") + 
  xlab("valeurs ajustées")
  
g2 <- ggplot(data = assur_lm, 
             aes(x = imc, y = resid)) + 
  geom_point(aes(col = fumeur)) +
  geom_smooth() +
  theme(legend.position = "bottom") + 
  ylab("résidus ordinaires") + 
  xlab("indice de masse corporelle")

g3 <- ggplot(data = assur_lm, 
             aes(x = factor(enfant), y = resid)) + 
  see::geom_violindot() + 
  xlab("nombre d'enfants") + 
  ylab("résidus ordinaires")
# Semblant d'augmentation

g4 <- ggplot(data = assur_lm, 
             aes(x = ajustee, y = abs(rstud))) +
  geom_point() + 
  ylab("|résidus studentisés externes|") + 
  xlab("valeurs ajustées")

(g1 + g2) / (g3 + g4)

g5 <- ggplot(data = assur_lm, 
             aes(x = fumobese, y = rstud)) +
  see::geom_violindot() + 
  ylab("résidus studentisés externes") + 
  xlab("indicateur fumeur / obèse")

di <- "t"
dp <- list(df = mod$df.residual)
g6 <- ggplot(data = assur_lm, mapping = aes(sample = rstud)) +
  # stat_qq_band(distribution = di, dparams = dp, detrend = TRUE, bandType = "boot", B = 9999) +
  stat_qq_line(distribution = di, dparams = dp, detrend = TRUE) +
  stat_qq_point(distribution = di, dparams = dp, detrend = TRUE) +
  labs(x = "quantiles théoriques", 
       y = "quantiles empiriques")
# Ici, df>1000, donc on pourrait utiliser l'approximation normale
# La bande de confiance est étrange parce que la droite passe 
# par les quartiles- à cause des valeurs aberrantes, la normalisation
# moyenne-variance donne des estimés TRÈS différents
# la faute à une implémentation qui emploi un estimateur robuste
# pour éliminer la traîne, mais pas de manière consistente...

car::qqPlot(rstudent(mod), 
            ylab = "quantiles studentisés externes",
              distribution = "t", df = mod$df.residual)

g7 <- ggplot(data = assur_lm, mapping = aes(x = rstud)) +
  geom_density() + 
  geom_histogram(aes(y = ..density..), 
                 bins = 30, 
                 alpha = 0.5)


# Corrélogramme
library(forecast)
data(trafficaerien, package = 'hecmodstat')
mod_ts <- lm(data = trafficaerien,
   log(passagers) ~ mois + annee)
resid(mod_ts) %>% 
  Acf(plot = FALSE) %>% 
  autoplot + 
  labs(title = "résidus ordinaires", 
         x = "décalage", 
         y = "autocorrélation")

