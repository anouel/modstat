library(hecmodstat)

# Ajuster le modèle linéaire avec "lm"
# Tableau des estimés des coefficients avec "summary"
# Tableau des statistiques F (SS3) avec "Anova" du paquet "car" 
# Détails sur les différentes paramétrisations https://mcfromnz.wordpress.com/2011/03/02/anova-type-iiiiii-ss-explained/
ols <- lm(intention ~ fixation, data = intention)
summary(ols) # 
confint(ols) # intervalles de confiance pour les coefficients
car::Anova(ols, type = 3)

intention$sexe <- relevel(intention$sexe, ref = "femme")
mco_complet <- lm(intention ~ fixation + emotion + sexe + educ + statut + revenu, data = intention)
summary(mco_complet)
car::Anova(mco_complet, type = 3) 


# Test-t pour deux échantillons
summary(lm(offre ~ groupe, data = billets))
#la valeur-p et la statistique coincides avec la sortie du test-t
t.test(offre ~ groupe, data = billets, var.equal = TRUE)

