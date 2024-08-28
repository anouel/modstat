library(hecmodstat)
library(ggplot2)
library(poorman)

# Question 1.2
# Taux de couverture
summarise(renfe_simu, 
          couverture = mean((icbi < -0.28) & (icbs > -0.28)))
# Histogramme des différences de prix selon la destination
ggplot(data = renfe_simu,
       aes(x = difmoy)) + 
  geom_histogram(bins = 30) + 
  geom_vline(xintercept = -0.28, col = "blue") + 
  xlab("différence moyenne de prix (en euros)") + 
  ylab("décompte")
# Puissance du test (puisque l'alternative est vraie)
summarise(renfe_simu, puissance = mean(valp < 0.05))
# Alternative (code)
# with(renfe_simu, mean(valp < 0.05))
# mean(renfe_simu$valp < 0.05)

# Question 1.3
with(renfe, 
     t.test(x = prix, mu = 43.25, conf.level = 0.9, subset = type %in% "AVE-TGV"))

# Question 1.4
summary(assurance)
assurance <- assurance %>% 
  mutate(obesite = factor(imc >= 30, labels = c("normal","obese"))
)


library(patchwork)
library(ggplot2)
# theme_set(theme_bw)

# Analyse exploratoire graphique des données
g1 <- ggplot(data = assurance) +
  geom_histogram(bins = 30, aes(x = log(frais), y = ..density..)) + 
  # geom_rug(sides = "b") +
  labs(x = "primes (en log dollars USD)",
       y = "densité")

g2 <- ggplot(data = assurance, aes(y = frais, x=region)) +
  geom_boxplot() + 
  labs(y = "primes (en dollars USD)",
       x = "région")
g3 <- ggplot(data = assurance, aes(y = frais, x=fumeur)) +
  geom_boxplot() + 
  labs(y = "primes (en dollars USD)",
       x = "fumeur")
g4 <- ggplot(data = assurance, aes(y = frais, x=imc, col = fumeur)) +
  geom_point() + 
  labs(y = "primes (en dollars USD)",
       x = "indice de masse corporel (en kg/m2)",
       col = "fumeur") + 
  geom_vline(xintercept = 30) + 
  theme(legend.position = "bottom")

g5 <- ggplot(data = assurance, 
             aes(y = frais, x=age, col = interaction(fumeur,obesite))) +
  geom_point() + 
  labs(y = "primes (en dollars USD)",
       x = "âge (en années)",
       col = "fumeur/obésité") + 
  theme(legend.position = "bottom")

g1 / (g2 + g3) / (g4 + g5)

# Comparaison des primes fumeurs et non-fumeurs
t.test(frais ~ fumeur, 
       data = assurance, 
       alternative = "less")


# Intervalles de confiance unilatéraux pour différence fumeurs imc >=30 ou imc <30
# `sapply` est l'équivalent d'une boucle
# pour chaque élément du vecteur, on applique la fonction
# retourne un vecteur/cube
sapply(c(0.9, 0.95, 0.99), 
       function(niveau){
      with(assurance, 
          t.test(frais ~ obesite, 
            subset = fumeur == "oui", 
            alternative = "less", 
            conf.level=niveau)$conf.int[2])})


