library(survival)
data(melanome, package = "hecmodstat")
summary(melanome)
str(melanome)

with(melanome, table(statut, ulcere))
# Modèle à risque proportionnels de Cox
mod1 <- coxph(Surv(temps, statut) ~ sexe + age + epaisseur +
                ulcere, data = melanome, ties="exact")
summary(mod1)
#Intervalles de confiance de Wald pour les coefficients
confint(mod1)

# Interaction entre les variables explicatives et log(temps)
# Le test pour la significativité de la valeur explicative
cox.zph(mod1)

# Graphiques des résidus en fonction du temps avec traîne locale
par(mfrow = c(2,2), mar = c(4,4,1,1))
plot(cox.zph(mod1), xlab = "temps")
# On a certaines indications que l'hypothèse de risque proportionnelle ne tient pas la route
# Les valeurs d'épaisseur associées à une survie plus courte...
dev.off()