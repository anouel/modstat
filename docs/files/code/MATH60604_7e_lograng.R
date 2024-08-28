library(survival)
library(coin)
data(cancersein, package = "hecmodstat")
# test du khi-deux pour le log des rangs

#valeur-p du test asymptotique
survdiff(Surv(temps, mort) ~ repimmuno, data = cancersein)

# valeur-p exact par simulation
# (pour petits échantillons, utiliser distribution="exact")
coin::logrank_test(Surv(temps, mort) ~ factor(repimmuno), 
                   data = cancersein, 
                   distribution = coin::approximate(nresample=1e5))

# Le test du log-rang calcule si les deux courbes sont identiques
# Idée de base: sous H0, les étiquettes 'repimmuno' ne sont pas pertinentes
# On peut les permuter et comparer les courbes

# Graphique avec plusieurs courbes
plot(survfit(Surv(temps, mort) ~ repimmuno, data = cancersein), 
     conf.int = FALSE,
     col = c(2,4), # rouge, bleu
     xlab = "temps", 
     ylab = "fonction de survie", 
     bty = "l")

# modèle de risque proportionnel de Cox
cox1 <- coxph(Surv(temps, mort) ~ repimmuno, data = cancersein)
summary(cox1)
## Le test du log-rang est le même que le test du score du modèle de Cox.
