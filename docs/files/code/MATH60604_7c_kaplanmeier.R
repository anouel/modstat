library(survival)
library(survminer) # graphiques

data(cancersein, package = "hecmodstat")
summary(cancersein)
with(cancersein, table(mort, repimmuno))

# Estimateur de Kaplan-Meier
# La réponse est le temps de survie et l'indicateur de censure 
# "0/FALSE" pour censuré à droite, "1/TRUE" pour événement observé
kapm1 <- survfit(Surv(temps, mort) ~ 1, 
                type = "kaplan-meier", 
                conf.type="log", data = cancersein)

summary(kapm1)
plot(kapm1, ylab = "fonction de survie", xlab = "temps (en jours)") 
ggsurvplot(fit = kapm1,
           conf.int = TRUE, 
           data = cancersein,
           legend = "none", 
           ylab = "probabilité de survie",
           xlab = "temps")
quantile(kapm1)


data(allaitement, package = "hecmodstat")
kapm2 <- survfit(Surv(duree, censure) ~ 1, 
                 type="kaplan-meier", 
                 conf.type="log", data = allaitement)

plot(kapm2, ylab = "fonction de survie", xlab = "durée (en semaines)") 
