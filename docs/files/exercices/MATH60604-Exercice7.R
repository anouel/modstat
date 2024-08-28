library(survival)
# Exercice 7.1
data(allaitement, package = "hecmodstat")
mod1 <- survfit(Surv(duree, censure) ~ fumeur, 
                type="kaplan-meier", 
                conf.type="log", data = allaitement)
plot(mod1, 
     conf.int = TRUE, 
     col = c(2,4), 
     yaxs= "i", 
     ylab = "fonction de survie", 
     xlab = "durée de l'allaitement (en semaines)", 
     bty = "l")

# Survie estimée à 36 semaines
mod1$surv[mod1$time == 36]
summary(mod1)
# Moyenne et médiane
quantile(mod1, 0.5)$quantile
print(mod1, print.rmean=TRUE)
# Ici, la moyenne restreinte correspond à l'estimé
# de la moyenne (aire sous la courbe) parce que la
# plus grande observation est un temps de 
# défaillance observé

# Tester l'égalité des fonctions de survie
survdiff(Surv(duree, censure) ~ fumeur, data = allaitement)

mod2 <- coxph(Surv(duree, censure) ~ pauvrete + agemere + fumeur + scolarite, 
      data = allaitement, ties = "exact")
summary(mod2)

# Exercice 7.2
data(shoes, package = "hecmodstat")
# Dans R, on donne un vecteur avec 
# FALSE=censure à droite, TRUE=temps de défaillance observé
mod3 <- survfit(Surv(temps, statut == "0") ~ 1, 
               type="kaplan-meier", 
               conf.type="log", data = chaussures)
quantile(mod3)$quantile
plot(mod3)
summary(mod3)

mod4 <- coxph(Surv(time, status == "0") ~ sexe + prix,
              data = chaussures)
summary(mod4)