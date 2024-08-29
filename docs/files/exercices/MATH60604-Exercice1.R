library(hecmodstat)
library(ggplot2)
library(poorman)


## Exercice 1.1
data(renfe, package = "hecmodstat")
# Sélectionner un sous-échantillon de trains à haute vitesse
donnees <- renfe |> filter(type %in% c("AVE","AVE-TGV"))
# Transformer la durée en heures
duree <- donnees$duree / 60
# Compute summary statistics of the duration
moy_d <- mean(duree);

# Étude de puissance
B <- 1e4L # nombre de simulations (10 000)
n <- nrow(donnees)
m <- 100L # taille du sous-échantillon
alpha <- 0.05 # niveau du test
# Créer une grille de durées pour le temps moyen sous l'alternative
temps <- sort(c(moy_d, seq(from = 2.83, to = 3, by = 0.005)))
# Créer un contenant pour stocker les résultats
puissance <- matrix(nrow = B, ncol = length(temps))
# Fixer le germe aléatoire pour s'assurer de résultats reproductibles
set.seed(2020)
for(i in seq_len(B)){ # pour chaque réplication
  for(j in seq_along(temps)){ # pour chaque alternative
    # Test-t pour un échantillon
    puissance[i,j] <-  t.test(x = duree[sample.int(n = n, size = m)],
                          mu = temps[j])$p.value
    # Stocker les valeurs-p
  }
}
# Créer un graphique de la puissance
ggplot(data = data.frame(temps = temps,
                         puissance = colMeans(puissance < alpha)),
       mapping = aes(x = temps, y = puissance)) +
  geom_point() +
  geom_vline(xintercept = 2.845, linetype = "dashed") +
  geom_hline(yintercept = alpha) +
  labs(x = "durée moyenne de trajet (en heures)",
       y = "",
       subtitle = "Puissance du test-t en fonction de la durée moyenne de trajet postulée.") +
  scale_y_continuous(limits = c(0,1),
                     expand = expansion(add = 0.01)) +
  theme_classic()


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
     t.test(x = prix, mu = 43.25, conf.level = 0.9, subset = type == "AVE-TGV"))
