library(hecmodstat)
library(ggplot2)
library(patchwork)
# Statistiques descriptives et tableaux de fréquences pour les variables catégorielles (facteurs)
str(intention)
summary(intention)

g1 <- ggplot(data = intention, aes(x=intention)) + 
  geom_bar()
g2 <- ggplot(data = intention, aes(x=age)) + 
  geom_histogram(bins = 10)
g3 <- ggplot(data = intention, aes(x=emotion)) + 
  geom_histogram(bins = 10)
g4 <- ggplot(data = intention, aes(x=fixation)) + 
  geom_histogram(bins = 10)
(g1 + g2) / (g3 + g4)


g5 <- ggplot(data = intention, 
             aes(x=fixation, y = intention)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x", se = FALSE) + 
  xlab("temps de fixation (en secondes)") + 
  ylab("intention d'achat")
g6 <- ggplot(data = intention, 
             aes(x=emotion, y = intention)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x", se = FALSE) + 
  xlab("score pour l'émotion") + 
  ylab("intention d'achat")
g5 + g6

lm(intention ~ fixation, data = intention)
lm(intention ~ sexe, data = intention)

# Créer manuellement les variables catégorielles avec des indicateurs
# la catégorie de référence est la première valeur alphanumérique
# utiliser 'relevel' pour la changer
educ2 <- as.integer(intention$educ == 2)
educ3 <- as.integer(intention$educ == 3)

lm(intention ~ educ, data = intention)
lm(intention ~ educ2 + educ3, data = intention)

lm(intention ~ sexe + age + revenu + educ + statut + fixation + emotion, data = intention)
