library(ggplot2)
library(patchwork)

data(interaction, package = "hecmodstat")
interaction$sexe = factor(interaction$sexe, labels = c("homme","femme"))
mod1 <- lm(intention ~ sexe + fixation, data = interaction)
# Dans R, a*b = a + b + a:b
mod2 <- lm(intention ~ sexe*fixation, data = interaction)
# Test F pour comparer deux modèles emboîtés
anova(mod1, mod2)

# Nuage de point, avec interactions
g2 <- ggplot(data = interaction, 
       aes(x = fixation, y = intention, col = sexe)) + 
  geom_point() + 
  geom_smooth(method = "lm", 
              se = FALSE, 
              formula = "y ~ x",
              show.legend = FALSE,
              fullrange = TRUE) 

# Modèle sans interaction
g1 <- ggplot(data = interaction, 
       aes(x = fixation, y = intention)) + 
  geom_point(aes(col = sexe)) + 
  geom_smooth(method = "lm", 
              se = FALSE, 
              formula = "y ~ x", 
              col = "black", 
              fullrange = TRUE)

(g1 + g2) + plot_layout(guides = 'collect') & theme(legend.position = "bottom")

# Graphique des résidus ordinaires, avec points colorés selon le sexe
# Moyenne globale de zéro, mais une traîne positive/négative bien visible selon le sexe
ggplot(data.frame(x = interaction$fixation, y = resid(mod1), sexe = interaction$sexe),
     aes(x = x, y = y, col = sexe)) + 
  geom_point() + 
  xlab("temps de fixation (en secondes)") + 
  ylab("résidus ordinaires") + 
  theme(legend.position = "bottom")

